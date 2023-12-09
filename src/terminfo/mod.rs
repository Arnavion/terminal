// Tiny terminfo parser.
//
// Ref:
//
// - Interpretation: `man 5 terminfo`
// - Binary compiled format: `man 5 term`
// - Undocumented details: https://github.com/mirror/ncurses/blob/61b684e2d53473d0262f28db0b0020e466bb8447/ncurses/tinfo/read_entry.c

#![deny(
	// clippy::arithmetic_side_effects, // Ensure all arithmetic checks for overflow. Disabled because it also fires on Checked<usize>::+, which is counter-productive.
	clippy::indexing_slicing, // Ensure all indexing is fallible.
)]

mod known_overrides;
pub use known_overrides::Overrides;

pub mod parameterized;

pub struct RawTerminfo {
	terminal_names: Box<str>,

	boolean_capabilities: Box<[Capability<bool>]>,
	number_capabilities: Box<[Capability<u32>]>,
	string_capabilities: Box<[Capability<TrustedRange<StringsTable>>]>,
	strings_table: StringsTable,

	extended_boolean_capabilities: Box<[(TrustedRange<ExtendedStringsTable>, bool)]>,
	extended_number_capabilities: Box<[(TrustedRange<ExtendedStringsTable>, u32)]>,
	extended_string_capabilities: Box<[(TrustedRange<ExtendedStringsTable>, Capability<TrustedRange<ExtendedStringsTable>>)]>,
	extended_strings_table: ExtendedStringsTable,
}

#[derive(Clone, Copy, Debug)]
pub enum Capability<T> {
	Value(T),
	Absent,
	Canceled,
}

// Newtype wrappers for the two strings tables. The newtype is used to parameterize TrustedRanges
// to ensure that a range is only used with the table that it was derived from.

#[repr(transparent)]
struct StringsTable(Box<[u8]>);

#[repr(transparent)]
struct ExtendedStringsTable(Box<[u8]>);

pub struct Terminfo {
	inner: RawTerminfo,
	cached_capabilities: CachedCapabilities,
	overridden_string_capabilities: std::collections::BTreeMap<usize, Capability<&'static [u8]>>,
	overridden_extended_string_capabilities: std::collections::BTreeMap<&'static [u8], Capability<&'static [u8]>>,
}

#[derive(Default)]
struct CachedCapabilities {
	alternate_screen: CacheEntry<(Box<[u8]>, Box<[u8]>)>,
	clear_line: CacheEntry<Box<[u8]>>,
	clear_screen: CacheEntry<Box<[u8]>>,
	clear_scrollback: CacheEntry<Box<[u8]>>,
	hide_cursor: CacheEntry<(Box<[u8]>, Box<[u8]>)>,
	move_cursor: CacheEntry<Box<[parameterized::Expr]>>,
	no_wraparound: CacheEntry<(Box<[u8]>, Box<[u8]>)>,
	sync: CacheEntry<(Box<[u8]>, Box<[u8]>)>,
}

#[derive(Debug, Default)]
enum CacheEntry<T> {
	#[default]
	Unknown,
	Present(T),
	Absent,
}

impl RawTerminfo {
	pub fn from_env() -> Result<Self, FromEnvError> {
		let term = std::env::var_os("TERM").ok_or(FromEnvError::EnvVarNotSet)?;
		Self::from_term_name(term).map_err(FromEnvError::FromTermName)
	}

	pub fn from_term_name(term: impl AsRef<std::ffi::OsStr>) -> Result<Self, FromTermNameError> {
		// TODO: These depend on distro's configure options for ncurses. Make them compile-time options instead of hard-coding?
		const TERMINFO: &[u8] = b"/usr/share/terminfo";
		const TERMINFO_DIRS: &[u8] = b"/etc/terminfo:/usr/share/terminfo";

		fn split_paths(paths: &std::ffi::OsStr) -> impl Iterator<Item = &std::path::Path> {
			let paths = std::os::unix::ffi::OsStrExt::as_bytes(paths);
			paths
				.split(|&b| b == b':')
				.map(|path| {
					let path = if path.is_empty() { TERMINFO } else { path };
					let path: &std::ffi::OsStr = std::os::unix::ffi::OsStrExt::from_bytes(path);
					path.as_ref()
				})
		}

		let term = term.as_ref();

		let first_byte = std::os::unix::ffi::OsStrExt::as_bytes(term).first().ok_or(FromTermNameError::MalformedTermName)?;
		let dir_name: &std::ffi::OsStr = std::os::unix::ffi::OsStrExt::from_bytes(std::slice::from_ref(first_byte));

		let env_terminfo = std::env::var_os("TERMINFO");
		let env_terminfo_dirs = std::env::var_os("TERMINFO_DIRS");
		let home_terminfo = std::env::var_os("HOME").map(|home| {
			let mut path = std::path::PathBuf::from(home);
			path.push(".terminfo");
			path
		});
		let mut search_paths =
			env_terminfo.as_deref().into_iter().map(AsRef::as_ref)
			.chain(home_terminfo.as_deref())
			.chain(env_terminfo_dirs.as_deref().into_iter().flat_map(split_paths))
			.chain(split_paths(std::os::unix::ffi::OsStrExt::from_bytes(TERMINFO_DIRS)))
			.chain({
				let path: &std::ffi::OsStr = std::os::unix::ffi::OsStrExt::from_bytes(TERMINFO);
				std::iter::once(path.as_ref())
			});

		let (path, f) = loop {
			let search_path = search_paths.next().ok_or(FromTermNameError::NoFileFound)?;
			let mut term_path = search_path.join(dir_name);
			term_path.push(term);

			match std::fs::File::open(&term_path) {
				Ok(f) => break (term_path, f),
				Err(err) if err.kind() == std::io::ErrorKind::NotFound => continue,
				Err(err) => return Err(FromTermNameError::File { path: term_path, inner: err }),
			}
		};
		let mut f = std::io::BufReader::new(f);
		Self::parse(&mut f).map_err(|err| FromTermNameError::Parse { path, inner: err })
	}

	pub fn parse(f: &mut impl std::io::BufRead) -> Result<Self, ParseError> {
		let reader = Reader::new(f)?;

		let terminal_names_num_bytes = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let boolean_capabilities_num = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let number_capabilities_num = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let string_capabilities_num = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let strings_table_num_bytes = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);

		let mut terminal_names = vec![0_u8; terminal_names_num_bytes.0];
		f.read_exact(&mut terminal_names).map_err(ParseError::Io)?;
		let terminal_names =
			std::ffi::CString::from_vec_with_nul(terminal_names)
			.map_err(|_| ParseError::MalformedString)?
			.into_string()
			.map_err(ParseError::TerminalNameNotUtf8)?
			.into();

		let mut boolean_capabilities = vec![Capability::Absent; boolean_capabilities_num.0];
		for b in &mut boolean_capabilities {
			*b = read_boolean(f)?;
		}
		let boolean_capabilities = boolean_capabilities.into();

		read_nul_if_odd(f, (terminal_names_num_bytes + boolean_capabilities_num)?)?;

		let mut number_capabilities = vec![Capability::Absent; number_capabilities_num.0];
		for n in &mut number_capabilities {
			*n = reader.read_number(f)?;
		}
		let number_capabilities = number_capabilities.into();

		let mut string_capabilities_offsets = vec![Capability::Absent; string_capabilities_num.0];
		for offset in &mut string_capabilities_offsets {
			*offset = match read_short(f)? {
				value @ 0.. => Capability::Value(value),
				-1 => Capability::Absent,
				-2 => Capability::Canceled,
				value => return Err(ParseError::MalformedStringOffset(value)),
			};
		}

		let mut strings_table = StringsTable(vec![0_u8; strings_table_num_bytes.0].into());
		f.read_exact(&mut strings_table.0).map_err(ParseError::Io)?;
		let strings_table = strings_table;

		let mut string_capabilities = Vec::with_capacity(string_capabilities_offsets.len());
		for string_capabilities_offset in string_capabilities_offsets {
			let s = match string_capabilities_offset {
				Capability::Value(start) => {
					let start = Checked::<usize>(start.try_into().expect("non-negative i16 cannot fail to convert to usize"));
					let range = strings_table.get_cstr_range(start)?;
					Capability::Value(range)
				},
				Capability::Absent => Capability::Absent,
				Capability::Canceled => Capability::Canceled,
			};
			string_capabilities.push(s);
		}
		let string_capabilities = string_capabilities.into();

		let mut result = Self {
			terminal_names,

			boolean_capabilities,
			number_capabilities,
			string_capabilities,
			strings_table,

			extended_boolean_capabilities: Box::new([]),
			extended_number_capabilities: Box::new([]),
			extended_string_capabilities: Box::new([]),
			extended_strings_table: ExtendedStringsTable(Box::new([])),
		};

		// Undocumented detail: There is another even byte padding between strings table and extended header.
		match read_nul_if_odd(f, strings_table_num_bytes) {
			Ok(()) => (),
			Err(ParseError::Io(err)) if err.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(result),
			Err(err) => return Err(err),
		};

		let extended_boolean_capabilities_num = match read_short(f) {
			Ok(extended_boolean_capabilities_num) => extended_boolean_capabilities_num,
			Err(ParseError::Io(err)) if err.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(result),
			Err(err) => return Err(err),
		};
		let extended_boolean_capabilities_num = Checked::<usize>(extended_boolean_capabilities_num.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let extended_number_capabilities_num = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let extended_string_capabilities_num = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let extended_strings_table_num = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);
		let extended_strings_table_num_bytes = Checked::<usize>(read_short(f)?.try_into().map_err(ParseError::IntegerOutOfRange)?);

		let extended_capabilities_num = ((extended_boolean_capabilities_num + extended_number_capabilities_num)? + extended_string_capabilities_num)?;
		if (extended_string_capabilities_num + extended_capabilities_num)? != extended_strings_table_num {
			// Undocumented detail: This is known to happen in the case of ms-terminal, which has two canceled extended string capabilities.
			// These capabilities still have names but no values, so extended_strings_table_num is 2 less than extended_string_capabilities_num + extended_capabilities_num.
		}

		let mut extended_boolean_capabilities = vec![false; extended_boolean_capabilities_num.0];
		for b in &mut extended_boolean_capabilities {
			*b = match read_boolean(f)? {
				Capability::Value(value) => value,
				Capability::Absent | Capability::Canceled => return Err(ParseError::ExtendedCapabilityIsAbsentOrCanceled),
			};
		}

		// Undocumented detail: There is an even byte padding between extended boolean capabilities and extended number capabilities.
		read_nul_if_odd(f, extended_boolean_capabilities_num)?;

		let mut extended_number_capabilities = vec![0_u32; extended_number_capabilities_num.0];
		for n in &mut extended_number_capabilities {
			*n = match reader.read_number(f)? {
				Capability::Value(value) => value,
				Capability::Absent | Capability::Canceled => return Err(ParseError::ExtendedCapabilityIsAbsentOrCanceled),
			};
		}

		let mut extended_string_capabilities_offsets = vec![Capability::Absent; extended_string_capabilities_num.0];
		for offset in &mut extended_string_capabilities_offsets {
			*offset = match read_short(f)? {
				value @ 0.. => Capability::Value(value),
				-1 => Capability::Absent, // screen.putty-m1b contains an absent extended string capability
				-2 => Capability::Canceled, // ms-terminal contains a canceled extended string capability
				value => return Err(ParseError::MalformedStringOffset(value)),
			};
		}

		let mut extended_capabilities_names_offsets = vec![0_i16; extended_capabilities_num.0];
		for offset in &mut extended_capabilities_names_offsets {
			*offset = match read_short(f)? {
				value @ 0.. => value,
				value => return Err(ParseError::MalformedStringOffset(value)),
			};
		}

		let mut extended_strings_table = ExtendedStringsTable(vec![0_u8; extended_strings_table_num_bytes.0].into());
		f.read_exact(&mut extended_strings_table.0).map_err(ParseError::Io)?;
		let extended_strings_table = extended_strings_table;

		let mut extended_string_capabilities = Vec::with_capacity(extended_string_capabilities_offsets.len());
		for start in extended_string_capabilities_offsets {
			match start {
				Capability::Value(start) => {
					let start = Checked::<usize>(start.try_into().expect("non-negative i16 cannot fail to convert to usize"));
					let range = extended_strings_table.get_cstr_range(start)?;
					extended_string_capabilities.push(Capability::Value(range));
				},
				Capability::Absent => extended_string_capabilities.push(Capability::Absent),
				Capability::Canceled => extended_string_capabilities.push(Capability::Canceled),
			}
		}

		// Undocumented detail: Capabilities names offsets are based not from the start of the extended strings table but from the end of the last string capability.
		let extended_string_capabilities_end =
			extended_string_capabilities.iter()
			.filter_map(|range| if let Capability::Value(range) = range { Some(range.0.end) } else { None })
			.max()
			.map_or(Ok(Checked(0)), |end| Checked(end) + 1)?; // Ranges don't include the trailing \0, so add 1 to compensate.

		let mut extended_capabilities_names = Vec::with_capacity(extended_capabilities_names_offsets.len());
		for start in extended_capabilities_names_offsets {
			let start = Checked::<usize>(start.try_into().expect("non-negative i16 cannot fail to convert to usize"));
			let start = (start + extended_string_capabilities_end)?;
			let range = extended_strings_table.get_cstr_range(start)?;
			extended_capabilities_names.push(range);
		}

		let mut extended_capabilities_names = extended_capabilities_names.into_iter();
		let extended_boolean_capabilities = extended_boolean_capabilities.into_iter().zip(&mut extended_capabilities_names).map(|(value, name)| (name, value)).collect();
		let extended_number_capabilities = extended_number_capabilities.into_iter().zip(&mut extended_capabilities_names).map(|(value, name)| (name, value)).collect();
		let extended_string_capabilities = extended_string_capabilities.into_iter().zip(&mut extended_capabilities_names).map(|(value, name)| (name, value)).collect();
		assert_eq!(extended_capabilities_names.next(), None);

		result.extended_boolean_capabilities = extended_boolean_capabilities;
		result.extended_number_capabilities = extended_number_capabilities;
		result.extended_string_capabilities = extended_string_capabilities;
		result.extended_strings_table = extended_strings_table;

		Ok(result)
	}

	pub fn terminal_names(&self) -> impl Iterator<Item = &str> {
		self.terminal_names.split('|')
	}

	pub fn boolean_capabilities(&self) -> impl Iterator<Item = Capability<bool>> + '_ {
		self.boolean_capabilities.iter().copied()
	}

	pub fn number_capabilities(&self) -> impl Iterator<Item = Capability<u32>> + '_ {
		self.number_capabilities.iter().copied()
	}

	pub fn string_capabilities(&self) -> impl Iterator<Item = Capability<&[u8]>> {
		self.string_capabilities.iter()
			.map(|range| range.as_ref().map(|range| self.strings_table.get_cstr_bytes(range.clone())))
	}

	pub fn extended_boolean_capabilities(&self) -> impl Iterator<Item = (&[u8], bool)> {
		self.extended_boolean_capabilities.iter()
			.map(|(name_range, value)| {
				let name = self.extended_strings_table.get_cstr_bytes(name_range.clone());
				(name, *value)
			})
	}

	pub fn extended_number_capabilities(&self) -> impl Iterator<Item = (&[u8], u32)> {
		self.extended_number_capabilities.iter()
			.map(|(name_range, value)| {
				let name = self.extended_strings_table.get_cstr_bytes(name_range.clone());
				(name, *value)
			})
	}

	pub fn extended_string_capabilities(&self) -> impl Iterator<Item = (&[u8], Capability<&[u8]>)> {
		self.extended_string_capabilities.iter()
			.map(|(name_range, value_range)| {
				let name = self.extended_strings_table.get_cstr_bytes(name_range.clone());
				let value = value_range.as_ref().map(|value_range| self.extended_strings_table.get_cstr_bytes(value_range.clone()));
				(name, value)
			})
	}
}

impl Terminfo {
	pub fn from_env() -> Result<Self, FromEnvError> {
		let inner = RawTerminfo::from_env()?;
		Ok(Self::with_overrides(inner, known_overrides::KNOWN_OVERRIDES))
	}

	pub fn from_term_name(term: impl AsRef<std::ffi::OsStr>) -> Result<Self, FromTermNameError> {
		let inner = RawTerminfo::from_term_name(term)?;
		Ok(Self::with_overrides(inner, known_overrides::KNOWN_OVERRIDES))
	}

	pub fn parse(f: &mut impl std::io::BufRead) -> Result<Self, ParseError> {
		let inner = RawTerminfo::parse(f)?;
		Ok(Self::with_overrides(inner, known_overrides::KNOWN_OVERRIDES))
	}

	pub fn new(inner: RawTerminfo) -> Self {
		Self::with_overrides(inner, known_overrides::KNOWN_OVERRIDES)
	}

	pub fn with_overrides(inner: RawTerminfo, overrides: &[(&str, Overrides)]) -> Self {
		let mut result = Self {
			inner,
			cached_capabilities: Default::default(),
			overridden_string_capabilities: Default::default(),
			overridden_extended_string_capabilities: Default::default(),
		};

		for name in result.inner.terminal_names() {
			if let Some(overrides) = overrides.iter().find_map(|&(term, overrides)| (term == name).then_some(overrides)) {
				result.overridden_string_capabilities.extend(overrides.string_capabilities.iter().copied());
				result.overridden_extended_string_capabilities.extend(overrides.extended_string_capabilities.iter().copied());
			}
		}

		result
	}

	pub fn terminal_names(&self) -> impl Iterator<Item = &str> {
		self.inner.terminal_names()
	}

	pub fn boolean_capabilities(&self) -> impl Iterator<Item = Capability<bool>> + '_ {
		self.inner.boolean_capabilities()
	}

	pub fn number_capabilities(&self) -> impl Iterator<Item = Capability<u32>> + '_ {
		self.inner.number_capabilities()
	}

	pub fn string_capabilities(&self) -> impl Iterator<Item = Capability<&[u8]>> {
		self.inner.string_capabilities()
			.enumerate()
			.map(|(i, value)| self.overridden_string_capabilities.get(&i).copied().unwrap_or(value))
	}

	pub fn extended_boolean_capabilities(&self) -> impl Iterator<Item = (&[u8], bool)> {
		self.inner.extended_boolean_capabilities()
	}

	pub fn extended_number_capabilities(&self) -> impl Iterator<Item = (&[u8], u32)> {
		self.inner.extended_number_capabilities()
	}

	pub fn extended_string_capabilities(&self) -> impl Iterator<Item = (&[u8], Capability<&[u8]>)> {
		self.inner.extended_string_capabilities()
			.filter(|(name, _)| !self.overridden_extended_string_capabilities.contains_key(name))
			.chain(self.overridden_extended_string_capabilities.iter().map(|(&name, &value)| (name, value)))
	}
}

macro_rules! terminfo_caching_method_enable_disable {
	($name:ident => [ $enable:literal, $disable:literal ]) => {
		impl Terminfo {
			pub fn $name(&mut self) -> (&[u8], &[u8]) {
				loop {
					match self.cached_capabilities.$name {
						CacheEntry::Unknown => {
							let enable = self.string_capabilities().nth($enable);
							let disable = self.string_capabilities().nth($disable);

							if let (Some(Capability::Value(enable)), Some(Capability::Value(disable))) = (enable, disable) {
								self.cached_capabilities.$name = CacheEntry::Present((enable.into(), disable.into()));
							}
							else {
								self.cached_capabilities.$name = CacheEntry::Absent;
							}
						},

						CacheEntry::Present((ref enable, ref disable)) => {
							break (enable, disable);
						},

						CacheEntry::Absent => break (b"", b""),
					}
				}
			}
		}
	};
}

macro_rules! terminfo_caching_method_single {
	($name:ident => [ $i:literal ]) => {
		impl Terminfo {
			pub fn $name(&mut self) -> &[u8] {
				loop {
					match self.cached_capabilities.$name {
						CacheEntry::Unknown => {
							let value = self.string_capabilities().nth($i);

							if let Some(Capability::Value(value)) = value {
								self.cached_capabilities.$name = CacheEntry::Present(value.into());
							}
							else {
								self.cached_capabilities.$name = CacheEntry::Absent;
							}
						},

						CacheEntry::Present(ref value) => break value,

						CacheEntry::Absent => break b"",
					}
				}
			}
		}
	};
}

macro_rules! terminfo_caching_method_extended_single {
	($name:ident => [ $i:literal ]) => {
		impl Terminfo {
			pub fn $name(&mut self) -> &[u8] {
				loop {
					match self.cached_capabilities.$name {
						CacheEntry::Unknown => {
							let value =
								self.extended_string_capabilities()
								.find_map(|(name, value)| (name == $i).then_some(value));

							if let Some(Capability::Value(value)) = value {
								self.cached_capabilities.$name = CacheEntry::Present(value.into());
							}
							else {
								self.cached_capabilities.$name = CacheEntry::Absent;
							}
						},

						CacheEntry::Present(ref value) => break value,

						CacheEntry::Absent => break b"",
					}
				}
			}
		}
	};
}

// TODO: Parse indices from term.h instead of hard-coding? But that requires ncurses-devel to be installed.
// Hard-coded values are correct for Linux anyway.
terminfo_caching_method_enable_disable!(alternate_screen => [28, 40]);
terminfo_caching_method_single!(clear_line => [6]);
terminfo_caching_method_single!(clear_screen => [5]);
terminfo_caching_method_extended_single!(clear_scrollback => [b"E3"]);
terminfo_caching_method_enable_disable!(hide_cursor => [13, 16]);
terminfo_caching_method_enable_disable!(no_wraparound => [152, 151]);

impl Terminfo {
	pub fn move_cursor(&mut self, row: i32, col: i32, out: &mut Vec<u8>) -> Result<(), ParameterizedStringError> {
		loop {
			match self.cached_capabilities.move_cursor {
				CacheEntry::Unknown => {
					let value = self.string_capabilities().nth(10);

					if let Some(Capability::Value(value)) = value {
						let expr = parameterized::parse(value).map_err(ParameterizedStringError::Parse)?;
						self.cached_capabilities.move_cursor = CacheEntry::Present(expr.into());
					}
					else {
						self.cached_capabilities.move_cursor = CacheEntry::Absent;
					}
				},

				CacheEntry::Present(ref expr) => {
					parameterized::eval(expr, &mut [row, col], out).map_err(ParameterizedStringError::Eval)?;
					break Ok(());
				},

				CacheEntry::Absent => break Ok(()),
			}
		}
	}
}

impl Terminfo {
	pub fn sync(&mut self) -> Result<(&[u8], &[u8]), ParameterizedStringError> {
		loop {
			match self.cached_capabilities.sync {
				CacheEntry::Unknown => {
					let value =
						self.extended_string_capabilities()
						.find_map(|(name, value)| (name == b"Sync").then_some(value));

					if let Some(Capability::Value(value)) = value {
						let expr = parameterized::parse(value).map_err(ParameterizedStringError::Parse)?;
						let mut begin = vec![];
						parameterized::eval(&expr, &mut [1], &mut begin).map_err(ParameterizedStringError::Eval)?;
						let mut end = vec![];
						parameterized::eval(&expr, &mut [2], &mut end).map_err(ParameterizedStringError::Eval)?;
						self.cached_capabilities.sync = CacheEntry::Present((begin.into(), end.into()));
					}
					else {
						self.cached_capabilities.sync = CacheEntry::Absent;
					}
				},

				CacheEntry::Present((ref begin, ref end)) => break Ok((begin, end)),

				CacheEntry::Absent => break Ok((b"", b"")),
			}
		}
	}
}

impl std::fmt::Debug for RawTerminfo {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f
			.debug_struct("RawTerminfo")
			.field("terminal_names", &self.terminal_names().collect::<Vec<_>>())
			.field("boolean_capabilities", &self.boolean_capabilities)
			.field("number_capabilities", &self.number_capabilities)
			.field("string_capabilities", &self.string_capabilities().map(|value| value.map(std::ffi::CString::new)).collect::<Vec<_>>())
			.field("extended_boolean_capabilities", &self.extended_boolean_capabilities().map(|(name, value)| (std::ffi::CString::new(name), value)).collect::<Vec<_>>())
			.field("extended_number_capabilities", &self.extended_number_capabilities().map(|(name, value)| (std::ffi::CString::new(name), value)).collect::<Vec<_>>())
			.field("extended_string_capabilities", &self.extended_string_capabilities().map(|(name, value)| (std::ffi::CString::new(name), value.map(std::ffi::CString::new))).collect::<Vec<_>>())
			.finish_non_exhaustive()
	}
}

impl std::fmt::Debug for Terminfo {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f
			.debug_struct("Terminfo")
			.field("inner", &self.inner)
			.finish_non_exhaustive()
	}
}

impl<T> Capability<T> {
	pub fn as_ref(&self) -> Capability<&T> {
		match self {
			Capability::Value(t) => Capability::Value(t),
			Capability::Absent => Capability::Absent,
			Capability::Canceled => Capability::Canceled,
		}
	}

	pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Capability<U> {
		match self {
			Capability::Value(t) => Capability::Value(f(t)),
			Capability::Absent => Capability::Absent,
			Capability::Canceled => Capability::Canceled,
		}
	}
}

struct TrustedRange<TStringsStorage>(std::ops::Range<usize>, std::marker::PhantomData<TStringsStorage>) where TStringsStorage: ?Sized;

impl<TStringsStorage> Clone for TrustedRange<TStringsStorage> where TStringsStorage: ?Sized {
	fn clone(&self) -> Self {
		TrustedRange(self.0.clone(), self.1)
	}
}

impl<TStringsStorage> std::fmt::Debug for TrustedRange<TStringsStorage> where TStringsStorage: ?Sized {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl<TStringsStorage> PartialEq<TrustedRange<TStringsStorage>> for TrustedRange<TStringsStorage> where TStringsStorage: ?Sized {
	fn eq(&self, other: &Self) -> bool {
		self.0 == other.0 && self.1 == other.1
	}
}

impl<TStringsStorage> Eq for TrustedRange<TStringsStorage> where TStringsStorage: ?Sized {}

trait StringsStorage {
	fn get_from(&self, start: usize) -> Option<&[u8]>;

	unsafe fn get_unchecked(&self, range: std::ops::Range<usize>) -> &[u8];

	fn get_cstr_range(&self, start: Checked<usize>) -> Result<TrustedRange<Self>, ParseError> {
		let s = self.get_from(start.0).ok_or(ParseError::StringOutOfBounds(start.0))?;
		let s = std::ffi::CStr::from_bytes_until_nul(s).map_err(|_| ParseError::MalformedString)?;
		let end = (start + s.to_bytes().len())?;
		let range = TrustedRange((start.0)..(end.0), Default::default());
		Ok(range)
	}

	fn get_cstr_bytes(&self, range: TrustedRange<Self>) -> &[u8] {
		unsafe {
			self.get_unchecked(range.0)
		}
	}
}

impl StringsStorage for StringsTable {
	fn get_from(&self, start: usize) -> Option<&[u8]> {
		self.0.get(start..)
	}

	unsafe fn get_unchecked(&self, range: std::ops::Range<usize>) -> &[u8] {
		self.0.get_unchecked(range)
	}
}

impl StringsStorage for ExtendedStringsTable {
	fn get_from(&self, start: usize) -> Option<&[u8]> {
		self.0.get(start..)
	}

	unsafe fn get_unchecked(&self, range: std::ops::Range<usize>) -> &[u8] {
		self.0.get_unchecked(range)
	}
}

enum Reader {
	V1,
	V2,
}

impl Reader {
	fn new(f: &mut impl std::io::Read) -> Result<Self, ParseError> {
		match read_short(f)? {
			0o432 => Ok(Reader::V1),
			0o1036 => Ok(Reader::V2),
			magic => Err(ParseError::UnknownVersion(magic)),
		}
	}

	fn read_number(&self, f: &mut impl std::io::Read) -> Result<Capability<u32>, ParseError> {
		match self {
			Reader::V1 => {
				let mut result = [0_u8; std::mem::size_of::<i16>()];
				f.read_exact(&mut result).map_err(ParseError::Io)?;
				let result = i16::from_le_bytes(result);
				match result {
					result @ 0.. => Ok(Capability::Value(result.try_into().expect("non-negative i16 cannot fail to convert to u32"))),
					-1 => Ok(Capability::Absent),
					-2 => Ok(Capability::Canceled),
					result => Err(ParseError::MalformedNumberCapability(result.into())),
				}
			},

			Reader::V2 => {
				let mut result = [0_u8; std::mem::size_of::<i32>()];
				f.read_exact(&mut result).map_err(ParseError::Io)?;
				let result = i32::from_le_bytes(result);
				match result {
					result @ 0.. => Ok(Capability::Value(result.try_into().expect("non-negative i32 cannot fail to convert to u32"))),
					-1 => Ok(Capability::Absent),
					-2 => Ok(Capability::Canceled),
					result => Err(ParseError::MalformedNumberCapability(result)),
				}
			},
		}
	}
}

fn read_boolean(f: &mut impl std::io::Read) -> Result<Capability<bool>, ParseError> {
	let mut result = [0_u8; std::mem::size_of::<u8>()];
	f.read_exact(&mut result).map_err(ParseError::Io)?;
	let result = u8::from_le_bytes(result);
	match result {
		0 => Ok(Capability::Value(false)),
		1 => Ok(Capability::Value(true)),
		0xfe => Ok(Capability::Canceled),
		b => Err(ParseError::MalformedBooleanCapability(b)),
	}
}

fn read_short(f: &mut impl std::io::Read) -> Result<i16, ParseError> {
	let mut result = [0_u8; std::mem::size_of::<i16>()];
	f.read_exact(&mut result).map_err(ParseError::Io)?;
	let result = i16::from_le_bytes(result);
	Ok(result)
}

fn read_nul_if_odd(f: &mut impl std::io::Read, num_bytes_read: Checked<usize>) -> Result<(), ParseError> {
	if num_bytes_read.0 % 2 == 1 {
		// Odd number of bytes read so far, so expect a \0
		let mut b = b'\0';
		f.read_exact(std::slice::from_mut(&mut b)).map_err(ParseError::Io)?;
		if b != b'\0' {
			return Err(ParseError::MalformedPadding(b));
		}
	}

	Ok(())
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
struct Checked<T>(T);

impl<T> std::fmt::Display for Checked<T> where T: std::fmt::Display {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl std::ops::Add<usize> for Checked<usize> {
	type Output = Result<Checked<usize>, ParseError>;

	fn add(self, rhs: usize) -> Self::Output {
		let result = self.0.checked_add(rhs).ok_or(ParseError::IntegerOverflow)?;
		Ok(Checked(result))
	}
}

impl std::ops::Add<Checked<usize>> for Checked<usize> {
	type Output = Result<Checked<usize>, ParseError>;

	fn add(self, rhs: Checked<usize>) -> Self::Output {
		let result = self.0.checked_add(rhs.0).ok_or(ParseError::IntegerOverflow)?;
		Ok(Checked(result))
	}
}

#[derive(Debug)]
pub enum FromEnvError {
	EnvVarNotSet,
	FromTermName(FromTermNameError),
}

#[derive(Debug)]
pub enum FromTermNameError {
	File { path: std::path::PathBuf, inner: std::io::Error },
	MalformedTermName,
	NoFileFound,
	Parse { path: std::path::PathBuf, inner: ParseError },
}

// TODO: Print file offset for location of error.
#[derive(Debug)]
pub enum ParseError {
	ExtendedCapabilityIsAbsentOrCanceled,
	IntegerOutOfRange(std::num::TryFromIntError),
	IntegerOverflow,
	Io(std::io::Error),
	MalformedBooleanCapability(u8),
	MalformedNumberCapability(i32),
	MalformedString,
	MalformedStringOffset(i16),
	MalformedPadding(u8),
	StringOutOfBounds(usize),
	TerminalNameNotUtf8(std::ffi::IntoStringError),
	UnknownVersion(i16),
}

#[derive(Debug)]
pub enum ParameterizedStringError {
	Eval(parameterized::EvalError),
	Parse(parameterized::ParseError),
}

impl std::fmt::Display for FromEnvError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			FromEnvError::EnvVarNotSet => f.write_str("TERM env var is not set to a valid value"),
			FromEnvError::FromTermName(inner) => write!(f, "could not parse terminfo: {inner}"),
		}
	}
}

impl std::error::Error for FromEnvError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			FromEnvError::EnvVarNotSet => None,
			FromEnvError::FromTermName(inner) => inner.source(),
		}
	}
}

impl std::fmt::Display for FromTermNameError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			FromTermNameError::File { path, inner } => write!(f, "could not open terminfo file {}: {inner}", path.display()),
			FromTermNameError::MalformedTermName => f.write_str("term name is malformed"),
			FromTermNameError::NoFileFound => f.write_str("no terminfo file found for given term name"),
			FromTermNameError::Parse { path, inner } => write!(f, "could not parse terminfo file {}: {inner}", path.display()),
		}
	}
}

impl std::error::Error for FromTermNameError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		#[allow(clippy::match_same_arms)]
		match self {
			FromTermNameError::File { path: _, inner } => inner.source(),
			FromTermNameError::MalformedTermName => None,
			FromTermNameError::NoFileFound => None,
			FromTermNameError::Parse { path: _, inner } => inner.source(),
		}
	}
}

impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ParseError::ExtendedCapabilityIsAbsentOrCanceled => f.write_str("extended capability is absent or canceled"),
			ParseError::IntegerOutOfRange(inner) => write!(f, "integer out of range: {inner}"),
			ParseError::IntegerOverflow => f.write_str("integer overflow"),
			ParseError::Io(inner) => write!(f, "I/O error: {inner}"),
			ParseError::MalformedBooleanCapability(value) => write!(f, "malformed boolean capability 0x{value:02x}"),
			ParseError::MalformedNumberCapability(value) => write!(f, "malformed number capability 0x{value:04x}"),
			ParseError::MalformedPadding(value) => write!(f, "malformed padding: expected 0x00 but got 0x{value:02x}"),
			ParseError::MalformedString => f.write_str("malformed C string"),
			ParseError::MalformedStringOffset(value) => write!(f, "malformed string offset 0x{value:02x}"),
			ParseError::StringOutOfBounds(start) => write!(f, "C string at {start}.. is out of bounds of strings table"),
			ParseError::TerminalNameNotUtf8(inner) => write!(f, "terminal name is not valid UTF-8: {inner}"),
			ParseError::UnknownVersion(value) => write!(f, "unknown terminfo version 0o{value:05o}"),
		}
	}
}

impl std::error::Error for ParseError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		#[allow(clippy::match_same_arms)]
		match self {
			ParseError::ExtendedCapabilityIsAbsentOrCanceled => None,
			ParseError::IntegerOutOfRange(inner) => inner.source(),
			ParseError::IntegerOverflow => None,
			ParseError::Io(inner) => inner.source(),
			ParseError::MalformedBooleanCapability(_) => None,
			ParseError::MalformedNumberCapability(_) => None,
			ParseError::MalformedPadding(_) => None,
			ParseError::MalformedString => None,
			ParseError::MalformedStringOffset(_) => None,
			ParseError::StringOutOfBounds(_) => None,
			ParseError::TerminalNameNotUtf8(inner) => inner.source(),
			ParseError::UnknownVersion(_) => None,
		}
	}
}

impl std::fmt::Display for ParameterizedStringError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ParameterizedStringError::Eval(inner) => write!(f, "parameterized string eval error: {inner}"),
			ParameterizedStringError::Parse(inner) => write!(f, "parameterized string parse error: {inner}"),
		}
	}
}

impl std::error::Error for ParameterizedStringError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		#[allow(clippy::match_same_arms)]
		match self {
			ParameterizedStringError::Eval(inner) => inner.source(),
			ParameterizedStringError::Parse(inner) => inner.source(),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::{parameterized, Capability, RawTerminfo, Terminfo};

	macro_rules! test_terms {
		(@inner $terminfo:ident { $($tests:tt)* } { }) => {
			$($tests)*
		};

		(@inner $terminfo:ident { $($tests:tt)* } { $method_name:ident ( $($params:tt)* ) $(.$unwrap:ident())? => $value:literal , $($rest:tt)* }) => {
			test_terms! {
				@inner
				$terminfo
				{
					$($tests)*

					{
						let mut value = vec![];
						$terminfo.$method_name($($params)*, &mut value)$(.$unwrap())?;
						assert_eq!(value.escape_ascii().to_string(), $value.escape_ascii().to_string());
					}
				}
				{ $($rest)* }
			}
		};

		(@inner $terminfo:ident { $($tests:tt)* } { $method_name:ident ( $($params:tt)* ) $(.$unwrap:ident())? = $value:literal , $($rest:tt)* }) => {
			test_terms! {
				@inner
				$terminfo
				{
					$($tests)*

					{
						let value = $terminfo.$method_name($($params)*)$(.$unwrap())?;
						assert_eq!(value.escape_ascii().to_string(), $value.escape_ascii().to_string());
					}
				}
				{ $($rest)* }
			}
		};

		(@inner $terminfo:ident { $($tests:tt)* } { $method_name:ident ( $($params:tt)* ) $(.$unwrap:ident())? = $enable:literal / $disable:literal , $($rest:tt)* }) => {
			test_terms! {
				@inner
				$terminfo
				{
					$($tests)*

					{
						let (enable, disable) = $terminfo.$method_name($($params:tt)*)$(.$unwrap())?;
						assert_eq!(enable.escape_ascii().to_string(), $enable.escape_ascii().to_string());
						assert_eq!(disable.escape_ascii().to_string(), $disable.escape_ascii().to_string());
					}
				}
				{ $($rest)* }
			}
		};

		(
			$(
				$test_name:ident ( $term:literal ) {
					$($tests:tt)*
				}
			)*
		) => {
			$(
				#[test]
				fn $test_name() {
					let terminfo = RawTerminfo::from_term_name($term).unwrap();
					let mut terminfo = Terminfo::new(terminfo);
					println!("{terminfo:?}");

					test_terms! {
						@inner
						terminfo
						{}
						{ $($tests)* }
					}
				}
			)*
		};
	}

	test_terms! {
		foot_extra("foot-extra") {
			alternate_screen() = b"\x1b[?1049h\x1b[22;0;0t" / b"\x1b[?1049l\x1b[23;0;0t",
			clear_line() = b"\x1b[K",
			clear_screen() = b"\x1b[H\x1b[2J",
			clear_scrollback() = b"\x1b[3J",
			hide_cursor() = b"\x1b[?25l" / b"\x1b[?12l\x1b[?25h",
			move_cursor(2, 0).unwrap() => b"\x1b[3;1H",
			no_wraparound() = b"\x1b[?7l" / b"\x1b[?7h",
			sync().unwrap() = b"\x1b[?2026h" / b"\x1b[?2026l",
		}

		ms_terminal("ms-terminal") {
			alternate_screen() = b"\x1b[?1049h\x1b[22;0;0t" / b"\x1b[?1049l\x1b[23;0;0t",
			clear_line() = b"\x1b[K",
			clear_screen() = b"\x1b[H\x1b[2J",
			clear_scrollback() = b"\x1b[3J",
			hide_cursor() = b"\x1b[?25l" / b"\x1b[?12l\x1b[?25h",
			move_cursor(2, 0).unwrap() => b"\x1b[3;1H",
			no_wraparound() = b"\x1b[?7l" / b"\x1b[?7h",
			sync().unwrap() = b"" / b"",
		}

		screen_putty_m1b("screen.putty-m1b") {
			alternate_screen() = b"\x1b[?1049h" / b"\x1b[?1049l",
			clear_line() = b"\x1b[K",
			clear_screen() = b"\x1b[H\x1b[J",
			clear_scrollback() = b"",
			hide_cursor() = b"\x1b[?25l" / b"\x1b[34h\x1b[?25h",
			move_cursor(2, 0).unwrap() => b"\x1b[3;1H",
			no_wraparound() = b"" / b"",
			sync().unwrap() = b"" / b"",
		}

		st("st") {
			alternate_screen() = b"\x1b[?1049h" / b"\x1b[?1049l",
			clear_line() = b"\x1b[K",
			clear_screen() = b"\x1b[H\x1b[2J",
			clear_scrollback() = b"",
			hide_cursor() = b"\x1b[?25l" / b"\x1b[?25h",
			move_cursor(2, 0).unwrap() => b"\x1b[3;1H",
			no_wraparound() = b"" / b"",
			sync().unwrap() = b"" / b"",
		}

		tmux_256color("tmux-256color") {
			alternate_screen() = b"\x1b[?1049h" / b"\x1b[?1049l",
			clear_line() = b"\x1b[K",
			clear_screen() = b"\x1b[H\x1b[J",
			clear_scrollback() = b"\x1b[3J",
			hide_cursor() = b"\x1b[?25l" / b"\x1b[34h\x1b[?25h",
			move_cursor(2, 0).unwrap() => b"\x1b[3;1H",
			no_wraparound() = b"\x1b[?7l" / b"\x1b[?7h",
			sync().unwrap() = b"" / b"",
		}

		xterm("xterm") {
			alternate_screen() = b"\x1b[?1049h\x1b[22;0;0t" / b"\x1b[?1049l\x1b[23;0;0t",
			clear_line() = b"\x1b[K",
			clear_screen() = b"\x1b[H\x1b[2J",
			clear_scrollback() = b"\x1b[3J",
			hide_cursor() = b"\x1b[?25l" / b"\x1b[?12l\x1b[?25h",
			move_cursor(2, 0).unwrap() => b"\x1b[3;1H",
			no_wraparound() = b"\x1b[?7l" / b"\x1b[?7h",
			sync().unwrap() = b"" / b"",
		}
	}

	#[test]
	fn current_term() {
		if std::env::var_os("TERM").is_some() {
			let terminfo = RawTerminfo::from_env().unwrap();
			println!("{terminfo:?}");
		}
	}

	#[test]
	fn all_terms() {
		fn parse_parameterized_string(s: &[u8]) -> Option<parameterized::ParseError> {
			if let Err(err) = parameterized::parse(s) {
				#[allow(clippy::match_same_arms, clippy::unnested_or_patterns)]
				match (err.expected, err.actual.as_deref()) {
					// Not actually a tparm'able string
					("printf flags or width or kind", None) |
					("printf kind", Some(r"\x0c")) |
					("printf kind", Some(r"\r")) |
					("printf kind", Some(r"\x0f")) |
					("printf kind", Some(r"\x1b")) |
					("printf kind", Some("$")) |
					("printf kind", Some(",")) |
					("printf kind", Some("E")) |
					("printf kind", Some("k")) |
					("printf kind", Some("n")) |
					("printf kind", Some("w")) |
					("printf kind", Some("y")) |
					("printf kind", Some("z")) |
					("printf kind", Some("[")) |
					("printf kind", Some("}")) |
					("b'}'", None) |
					(r#"b"%t""#, None) => None,

					// TODO: char constant
					("printf kind", Some(r"\'")) => None,

					_ => Some(err),
				}
			}
			else {
				None
			}
		}

		let mut errors = vec![];

		let admin_entries = if let Ok(entries) = std::fs::read_dir("/etc/terminfo") { Some(entries) } else { None };
		let distro_entries = if let Ok(entries) = std::fs::read_dir("/usr/share/terminfo") { Some(entries) } else { None };
		let entries = admin_entries.into_iter().flatten().chain(distro_entries.into_iter().flatten());
		for entry in entries {
			let Ok(entry) = entry else { continue; };
			let is_dir = if let Ok(ft) = entry.file_type() { ft.is_dir() } else { false };
			if !is_dir {
				continue;
			}

			let Ok(entries) = std::fs::read_dir(entry.path()) else { return; };
			for entry in entries {
				let Ok(entry) = entry else { continue; };
				let is_file = if let Ok(ft) = entry.file_type() { ft.is_file() } else { false };
				if !is_file {
					continue;
				}

				let Ok(f) = std::fs::File::open(entry.path()) else { continue; };
				let mut f = std::io::BufReader::new(f);
				let terminfo = match RawTerminfo::parse(&mut f) {
					Ok(terminfo) => terminfo,
					Err(err) => panic!("could not parse {}: {err}", entry.path().display()),
				};

				for value in terminfo.string_capabilities() {
					if let Capability::Value(value) = value {
						if let Some(err) = parse_parameterized_string(value) {
							errors.push(format!(
								"could not parse string capability of {} : {} : {err}",
								entry.path().display(), value.escape_ascii(),
							));
						}
					}
				}

				for (name, value) in terminfo.extended_string_capabilities() {
					if let Capability::Value(value) = value {
						if let Some(err) = parse_parameterized_string(value) {
							errors.push(format!(
								"could not parse string capability of {} : {} = {} : {err}",
								entry.path().display(), name.escape_ascii(), value.escape_ascii(),
							));
						}
					}
				}
			}
		}

		if !errors.is_empty() {
			for error in errors {
				eprintln!("{error}");
			}
			panic!("one or more errors");
		}
	}
}
