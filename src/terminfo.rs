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

pub struct Terminfo {
	terminal_names: String,

	boolean_capabilities: Vec<Capability<bool>>,
	number_capabilities: Vec<Capability<u32>>,
	string_capabilities: Vec<Capability<TrustedRange<StringsTable>>>,
	strings_table: StringsTable,

	extended_boolean_capabilities: Vec<(TrustedRange<ExtendedStringsTable>, bool)>,
	extended_number_capabilities: Vec<(TrustedRange<ExtendedStringsTable>, u32)>,
	extended_string_capabilities: Vec<(TrustedRange<ExtendedStringsTable>, Capability<TrustedRange<ExtendedStringsTable>>)>,
	extended_strings_table: ExtendedStringsTable,

	cached_capabilities: CachedCapabilities,
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
struct StringsTable(Vec<u8>);

#[repr(transparent)]
struct ExtendedStringsTable(Vec<u8>);

#[derive(Default)]
struct CachedCapabilities {
	alternate_screen: CacheEntry<(TrustedRange<StringsTable>, TrustedRange<StringsTable>)>,
	no_wraparound: CacheEntry<(TrustedRange<StringsTable>, TrustedRange<StringsTable>)>,
	clear_screen: CacheEntry<TrustedRange<StringsTable>>,
	clear_scrollback: CacheEntry<TrustedRange<ExtendedStringsTable>>,
}

#[derive(Debug)]
enum CacheEntry<T> {
	Unknown,
	Present(T),
	Absent,
}

impl Terminfo {
	pub fn from_env() -> Result<Terminfo, FromEnvError> {
		let term = std::env::var_os("TERM").ok_or(FromEnvError::EnvVarNotSet)?;
		Self::from_term_name(&term).map_err(FromEnvError::FromTermName)
	}

	pub fn from_term_name(term: impl AsRef<std::ffi::OsStr>) -> Result<Terminfo, FromTermNameError> {
		let term = term.as_ref();

		let first_byte = match std::os::unix::ffi::OsStrExt::as_bytes(term) {
			[first_byte, ..] => *first_byte,
			[] => return Err(FromTermNameError::MalformedTermName),
		};
		let dir_name: &std::ffi::OsStr = std::os::unix::ffi::OsStrExt::from_bytes(std::slice::from_ref(&first_byte));

		let (path, f) = {
			// TODO: `infocmp -D` gives the actual paths. Make them compile-time options instead of hard-coding?
			let user_terminfo_path = std::path::Path::new("/etc/terminfo");
			let distro_terminfo_path = std::path::Path::new("/usr/share/terminfo");

			let mut term_path = user_terminfo_path.join(dir_name);
			term_path.push(term);

			match std::fs::File::open(&term_path) {
				Ok(f) => (term_path, f),

				Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
					let mut term_path = distro_terminfo_path.join(dir_name);
					term_path.push(term);
					match std::fs::File::open(&term_path) {
						Ok(f) => (term_path, f),
						Err(err) => return Err(FromTermNameError::File { path: term_path, inner: err }),
					}
				},

				Err(err) => return Err(FromTermNameError::File { path: term_path, inner: err }),
			}
		};
		let mut f = std::io::BufReader::new(f);
		Self::parse(&mut f).map_err(|err| FromTermNameError::Parse { path, inner: err })
	}

	pub fn parse(f: &mut impl std::io::BufRead) -> Result<Terminfo, ParseError> {
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
			.map_err(ParseError::TerminalNameNotUtf8)?;

		let mut boolean_capabilities = vec![Capability::Absent; boolean_capabilities_num.0];
		for b in &mut boolean_capabilities {
			*b = read_boolean(f)?;
		}

		read_nul_if_odd(f, (terminal_names_num_bytes + boolean_capabilities_num)?)?;

		let mut number_capabilities = vec![Capability::Absent; number_capabilities_num.0];
		for n in &mut number_capabilities {
			*n = reader.read_number(f)?;
		}

		let mut string_capabilities_offsets = vec![Capability::Absent; string_capabilities_num.0];
		for offset in &mut string_capabilities_offsets {
			*offset = match read_short(f)? {
				value @ 0.. => Capability::Value(value),
				-1 => Capability::Absent,
				-2 => Capability::Canceled,
				value => return Err(ParseError::MalformedStringOffset(value)),
			};
		}

		let mut strings_table = vec![0_u8; strings_table_num_bytes.0];
		f.read_exact(&mut strings_table).map_err(ParseError::Io)?;
		let strings_table = StringsTable(strings_table);

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

		let mut result = Terminfo {
			terminal_names,

			boolean_capabilities,
			number_capabilities,
			string_capabilities,
			strings_table,

			extended_boolean_capabilities: vec![],
			extended_number_capabilities: vec![],
			extended_string_capabilities: vec![],
			extended_strings_table: ExtendedStringsTable(vec![]),

			cached_capabilities: Default::default(),
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

		let mut extended_strings_table = vec![0_u8; extended_strings_table_num_bytes.0];
		f.read_exact(&mut extended_strings_table).map_err(ParseError::Io)?;
		let extended_strings_table = ExtendedStringsTable(extended_strings_table);

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
			.unwrap_or(0);

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

	pub fn string_capabilities(&self) -> impl Iterator<Item = Capability<&std::ffi::CStr>> {
		self.string_capabilities.iter().map(|range| {
			match range {
				Capability::Value(range) => {
					let s = self.strings_table.get_cstr(range.clone());
					Capability::Value(s)
				},
				Capability::Absent => Capability::Absent,
				Capability::Canceled => Capability::Canceled,
			}
		})
	}

	pub fn extended_boolean_capabilities(&self) -> impl Iterator<Item = (&std::ffi::CStr, bool)> {
		self.extended_boolean_capabilities.iter()
			.map(|(name_range, value)| {
				let name = self.extended_strings_table.get_cstr(name_range.clone());
				(name, *value)
			})
	}

	pub fn extended_number_capabilities(&self) -> impl Iterator<Item = (&std::ffi::CStr, u32)> {
		self.extended_number_capabilities.iter()
			.map(|(name_range, value)| {
				let name = self.extended_strings_table.get_cstr(name_range.clone());
				(name, *value)
			})
	}

	pub fn extended_string_capabilities(&self) -> impl Iterator<Item = (&std::ffi::CStr, Capability<&std::ffi::CStr>)> {
		self.extended_string_capabilities.iter()
			.map(|(name_range, value_range)| {
				let name = self.extended_strings_table.get_cstr(name_range.clone());
				let value = match value_range {
					Capability::Value(value_range) => Capability::Value(self.extended_strings_table.get_cstr(value_range.clone())),
					Capability::Absent => Capability::Absent,
					Capability::Canceled => Capability::Canceled,
				};
				(name, value)
			})
	}
}

macro_rules! terminfo_caching_method_enable_disable {
	($name:ident => [ $enable:literal, $disable:literal ]) => {
		impl Terminfo {
			pub fn $name(&mut self) -> (&std::ffi::CStr, &std::ffi::CStr) {
				loop {
					match &mut self.cached_capabilities.$name {
						CacheEntry::Unknown => {
							let enable_range =
								self.string_capabilities.iter()
								.nth($enable)
								.and_then(|range| match range {
									Capability::Value(range) => Some(range),
									Capability::Absent | Capability::Canceled => None,
								})
								.cloned();
							let disable_range =
								self.string_capabilities.iter()
								.nth($disable)
								.and_then(|range| match range {
									Capability::Value(range) => Some(range),
									Capability::Absent | Capability::Canceled => None,
								})
								.cloned();

							if let (Some(enable_range), Some(disable_range)) = (enable_range, disable_range) {
								self.cached_capabilities.$name = CacheEntry::Present((enable_range, disable_range));
							}
							else {
								self.cached_capabilities.$name = CacheEntry::Absent;
							}
						},

						CacheEntry::Present((enable_range, disable_range)) => {
							let enable = self.strings_table.get_cstr(enable_range.clone());
							let disable = self.strings_table.get_cstr(disable_range.clone());
							break (enable, disable);
						},

						CacheEntry::Absent => break (EMPTY_CSTR, EMPTY_CSTR),
					}
				}
			}
		}
	};
}

macro_rules! terminfo_caching_method_single {
	($name:ident => [ $i:literal ]) => {
		impl Terminfo {
			pub fn $name(&mut self) -> &std::ffi::CStr {
				loop {
					match &mut self.cached_capabilities.$name {
						CacheEntry::Unknown => {
							let range =
								self.string_capabilities.iter()
								.nth($i)
								.and_then(|range| match range {
									Capability::Value(range) => Some(range),
									Capability::Absent | Capability::Canceled => None,
								})
								.cloned();

							if let Some(range) = range {
								self.cached_capabilities.$name = CacheEntry::Present(range);
							}
							else {
								self.cached_capabilities.$name = CacheEntry::Absent;
							}
						},

						CacheEntry::Present(range) => break self.strings_table.get_cstr(range.clone()),

						CacheEntry::Absent => break EMPTY_CSTR,
					}
				}
			}
		}
	};
}

macro_rules! terminfo_caching_method_extended_single {
	($name:ident => [ $i:literal ]) => {
		impl Terminfo {
			pub fn $name(&mut self) -> &std::ffi::CStr {
				loop {
					match &mut self.cached_capabilities.$name {
						CacheEntry::Unknown => {
							let range =
								self.extended_string_capabilities.iter()
								.find_map(|(name_range, value_range)| (self.extended_strings_table.get_cstr(name_range.clone()).to_bytes() == $i).then_some(value_range.clone()));

							if let Some(Capability::Value(range)) = range {
								self.cached_capabilities.$name = CacheEntry::Present(range);
							}
							else {
								self.cached_capabilities.$name = CacheEntry::Absent;
							}
						},

						CacheEntry::Present(range) => break self.extended_strings_table.get_cstr(range.clone()),

						CacheEntry::Absent => break EMPTY_CSTR,
					}
				}
			}
		}
	};
}

// TODO: Parse indices from term.h instead of hard-coding? But that requires ncurses-devel to be installed.
// Hard-coded values are correct for Linux anyway.
terminfo_caching_method_enable_disable!(alternate_screen => [28, 40]);
terminfo_caching_method_enable_disable!(no_wraparound => [152, 151]);
terminfo_caching_method_single!(clear_screen => [5]);
terminfo_caching_method_extended_single!(clear_scrollback => [b"E3"]);

impl std::fmt::Debug for Terminfo {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f
			.debug_struct("Terminfo")
			.field("terminal_names", &self.terminal_names().collect::<Vec<_>>())
			.field("boolean_capabilities", &self.boolean_capabilities)
			.field("number_capabilities", &self.number_capabilities)
			.field("string_capabilities", &self.string_capabilities().collect::<Vec<_>>())
			.field("extended_boolean_capabilities", &self.extended_boolean_capabilities().collect::<Vec<_>>())
			.field("extended_number_capabilities", &self.extended_number_capabilities().collect::<Vec<_>>())
			.field("extended_string_capabilities", &self.extended_string_capabilities().collect::<Vec<_>>())
			.finish_non_exhaustive()
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
		let s = from_bytes_until_nul(s)?;
		let end = (start + s.to_bytes_with_nul().len())?;
		let range = TrustedRange((start.0)..(end.0), Default::default());
		Ok(range)
	}

	fn get_cstr(&self, range: TrustedRange<Self>) -> &std::ffi::CStr {
		unsafe {
			let s = self.get_unchecked(range.0);
			let s = std::ffi::CStr::from_bytes_with_nul_unchecked(s);
			s
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

impl<T> Default for CacheEntry<T> {
	fn default() -> Self {
		CacheEntry::Unknown
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
	#[allow(clippy::match_same_arms)]
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

// TODO: Replace with std::ffi::CStr::from_bytes_until_nul when that is stabilized
fn from_bytes_until_nul(s: &[u8]) -> Result<&std::ffi::CStr, ParseError> {
	s.iter()
		.position(|&b| b == b'\0')
		.map(|nul_pos| unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(s.get_unchecked(..=nul_pos)) })
		.ok_or(ParseError::MalformedString)
}

const EMPTY_CSTR: &std::ffi::CStr = unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"\0") };

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
			FromTermNameError::Parse { path, inner } => write!(f, "could not parse terminfo file {}: {inner}", path.display()),
		}
	}
}

impl std::error::Error for FromTermNameError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			FromTermNameError::File { path: _, inner } => inner.source(),
			FromTermNameError::MalformedTermName => None,
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

#[cfg(test)]
mod tests {
	macro_rules! test_terms {
		(@inner $terminfo:ident { $($tests:tt)* } { }) => {
			$($tests)*
		};

		(@inner $terminfo:ident { $($tests:tt)* } { $method_name:ident = $value:literal , $($rest:tt)* }) => {
			test_terms! {
				@inner
				$terminfo
				{
					$($tests)*

					{
						let value = $terminfo.$method_name();
						assert_eq!(value, unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked($value) });
					}
				}
				{ $($rest)* }
			}
		};

		(@inner $terminfo:ident { $($tests:tt)* } { $method_name:ident = $enable:literal / $disable:literal , $($rest:tt)* }) => {
			test_terms! {
				@inner
				$terminfo
				{
					$($tests)*

					{
						let (enable, disable) = $terminfo.$method_name();
						assert_eq!(enable, unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked($enable) });
						assert_eq!(disable, unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked($disable) });
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
					let mut terminfo = super::Terminfo::from_term_name($term).unwrap();
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
		xterm("xterm") {
			alternate_screen = b"\x1b[?1049h\x1b[22;0;0t\0" / b"\x1b[?1049l\x1b[23;0;0t\0",
			no_wraparound = b"\x1b[?7l\0" / b"\x1b[?7h\0",
			clear_screen = b"\x1b[H\x1b[2J\0",
			clear_scrollback = b"\x1b[3J\0",
		}

		ms_terminal("ms-terminal") {
			alternate_screen = b"\x1b[?1049h\x1b[22;0;0t\0" / b"\x1b[?1049l\x1b[23;0;0t\0",
			no_wraparound = b"\x1b[?7l\0" / b"\x1b[?7h\0",
			clear_screen = b"\x1b[H\x1b[2J\0",
			clear_scrollback = b"\x1b[3J\0",
		}

		screen_putty_m1b("screen.putty-m1b") {
			alternate_screen = b"\x1b[?1049h\0" / b"\x1b[?1049l\0",
			no_wraparound = b"\0" / b"\0",
			clear_screen = b"\x1b[H\x1b[J\0",
			clear_scrollback = b"\0",
		}

		st("st") {
			alternate_screen = b"\x1b[?1049h\0" / b"\x1b[?1049l\0",
			no_wraparound = b"\0" / b"\0",
			clear_screen = b"\x1b[H\x1b[2J\0",
			clear_scrollback = b"\0",
		}
	}

	#[test]
	fn current_term() {
		if std::env::var_os("TERM").is_some() {
			let terminfo = super::Terminfo::from_env().unwrap();
			println!("{terminfo:?}");
		}
	}

	#[test]
	fn all_terms() {
		let user_entries = if let Ok(entries) = std::fs::read_dir("/etc/terminfo") { Some(entries) } else { None };
		let distro_entries = if let Ok(entries) = std::fs::read_dir("/usr/share/terminfo") { Some(entries) } else { None };
		let entries = user_entries.into_iter().flatten().chain(distro_entries.into_iter().flatten());
		for entry in entries {
			let entry = if let Ok(entry) = entry { entry } else { continue; };
			let is_dir = if let Ok(ft) = entry.file_type() { ft.is_dir() } else { false };
			if !is_dir {
				continue;
			}

			let entries = if let Ok(entries) = std::fs::read_dir(entry.path()) { entries } else { return; };
			for entry in entries {
				let entry = if let Ok(entry) = entry { entry } else { continue; };
				let is_file = if let Ok(ft) = entry.file_type() { ft.is_file() } else { false };
				if !is_file {
					continue;
				}

				let f = if let Ok(f) = std::fs::File::open(entry.path()) { f } else { continue; };
				let mut f = std::io::BufReader::new(f);
				if let Err(err) = super::Terminfo::parse(&mut f) {
					panic!("could not parse {}: {err}", entry.path().display());
				}
			}
		}
	}
}
