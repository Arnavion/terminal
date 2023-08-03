use super::{Cmp, Expr, Math, PrintfFlagLeft, PrintfFlags, PrintfFlagSign, PrintfKind};

pub fn parse(mut input: &[u8]) -> Result<Vec<Expr>, ParseError> {
	let original_len = input.len();
	match parse_expr_until(&mut input, &[]) {
		Ok(expr) => Ok(expr),
		Err(mut err) => {
			err.pos = Some(original_len - input.len());
			Err(err)
		},
	}
}

fn parse_expr_until(rest: &mut &[u8], until: &[&[u8]]) -> Result<Vec<Expr>, ParseError> {
	let mut result = vec![];

	loop {
		if until.iter().any(|&until| rest.starts_with(until)) {
			break;
		}

		if let Some(first) = split_first(rest) {
			match first {
				b'%' => result.push(parse_percent(rest)?),
				first => result.push(Expr::Literal(first)),
			}
		}
		else {
			break;
		}
	}

	Ok(result)
}

fn parse_percent(rest: &mut &[u8]) -> Result<Expr, ParseError> {
	match rest {
		[b'!', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::Not)) },

		[b'%', rest_ @ ..] => { *rest = rest_; Ok(Expr::Literal(b'%')) },

		[b'&', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::BitAnd)) },

		// TODO: char constant
		// [b'\'', rest_ @ ..] => { *rest = rest_; },

		[b'*', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::Mul)) },

		[b'+', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::Add)) },

		[b'-', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::Sub)) },

		[b'/', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::Div)) },

		[b'<', rest_ @ ..] => { *rest = rest_; Ok(Expr::Cmp(Cmp::Less)) },

		[b'=', rest_ @ ..] => { *rest = rest_; Ok(Expr::Cmp(Cmp::Equal)) },

		[b'>', rest_ @ ..] => { *rest = rest_; Ok(Expr::Cmp(Cmp::Greater)) },

		[b'?', rest_ @ ..] => {
			*rest = rest_;

			let cond = parse_expr_until(rest, &[b"%t"])?;
			if !split_start(rest, b"%t") {
				return Err((r#"b"%t""#, rest.first()).into());
			}

			parse_if(rest, cond)
		},

		[b'A', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::And)) },

		[b'O', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::Or)) },

		[b'P', rest_ @ ..] => {
			*rest = rest_;
			match split_first(rest) {
				Some(b @ (b'A'..=b'Z' | b'a'..=b'z')) => Ok(Expr::StoreVariable(b)),
				b => Err(("valid % directive", b).into()),
			}
		},

		[b'^', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::BitXor)) },

		[b'c', rest_ @ ..] => {
			*rest = rest_;
			Ok(Expr::Printf {
				flags: PrintfFlags {
					alternate_form: false,
					left: PrintfFlagLeft::Default,
					sign: PrintfFlagSign::Default,
				},
				kind: PrintfKind::Char,
				width_and_precision: None,
			})
		},

		[b'g', rest_ @ ..] => {
			*rest = rest_;
			match split_first(rest) {
				Some(b @ (b'A'..=b'Z' | b'a'..=b'z')) => Ok(Expr::LoadVariable(b)),
				b => Err(("valid % directive", b).into()),
			}
		},

		[b'i', rest_ @ ..] => { *rest = rest_; Ok(Expr::IncrementFirstTwoIntegerParams) },

		[b'l', rest_ @ ..] => { *rest = rest_; Ok(Expr::Strlen) },

		[b'm', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::Rem)) },

		[b'p', rest_ @ ..] => {
			*rest = rest_;
			let param_num = parse_param_num(rest)?;
			Ok(Expr::Param(param_num))
		},

		[b'{', rest_ @ ..] => {
			*rest = rest_;
			let integer = parse_integer(rest)?;
			Ok(Expr::Integer(integer))
		},

		[b'|', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::BitOr)) },

		[b'~', rest_ @ ..] => { *rest = rest_; Ok(Expr::Math(Math::BitNot)) },

		_ => parse_printf(rest),
	}
}

fn parse_param_num(rest: &mut &[u8]) -> Result<usize, ParseError> {
	match split_first(rest) {
		Some(b @ b'1'..=b'9') => Ok((b - b'1').into()),
		b => Err(("b'1'..=b'9'", b).into()),
	}
}

fn parse_integer(rest: &mut &[u8]) -> Result<i32, ParseError> {
	let end = rest.iter().position(|&b| b == b'}').unwrap_or(rest.len());

	// TODO: Use split_at_unchecked when that is stabilized
	let (s, rest_) = unsafe { (rest.get_unchecked(..end), rest.get_unchecked(end..)) };
	*rest = rest_;

	if !split_start(rest, b"}") {
		return Err(("b'}'", rest.get(end)).into());
	}

	let i =
		std::str::from_utf8(s)
		.map_err(|_| ("integer", Some(s)))?
		.parse()
		.map_err(|_| ("integer", Some(s)))?;
	Ok(i)
}

fn parse_if(rest: &mut &[u8], cond: Vec<Expr>) -> Result<Expr, ParseError> {
	let t = parse_expr_until(rest, &[b"%e", b"%;"])?;

	#[allow(clippy::if_same_then_else)]
	let e =
		if split_start(rest, b"%e") {
			let e = parse_expr_until(rest, &[b"%t", b"%;"])?;
			if split_start(rest, b"%t") {
				vec![parse_if(rest, e)?]
			}
			else if split_start(rest, b"%;") {
				e
			}
			// Undocumented detail: %; is optional if it's at the end of the string
			else if rest.is_empty() {
				e
			}
			else {
				return Err((r#"b"%t" | b"%;""#, rest.first()).into());
			}
		}
		else if split_start(rest, b"%;") {
			vec![]
		}
		// Undocumented detail: %; is optional if it's at the end of the string
		else if rest.is_empty() {
			vec![]
		}
		else {
			return Err((r#"b"%e" | b"%;""#, rest.first()).into());
		};
	Ok(Expr::If(cond, t, e))
}

fn parse_printf(rest: &mut &[u8]) -> Result<Expr, ParseError> {
	let _ = split_start(rest, b":");

	let mut flags = PrintfFlags {
		alternate_form: false,
		left: PrintfFlagLeft::Default,
		sign: PrintfFlagSign::Default,
	};
	loop {
		match rest {
			[b' ', rest_ @ ..] => { *rest = rest_; flags.sign = PrintfFlagSign::Space; },
			[b'#', rest_ @ ..] => { *rest = rest_; flags.alternate_form = true; },
			[b'+', rest_ @ ..] => { *rest = rest_; flags.sign = PrintfFlagSign::Plus; },
			[b'-', rest_ @ ..] => { *rest = rest_; flags.left = PrintfFlagLeft::Aligned; },
			[b'0', rest_ @ ..] => { *rest = rest_; flags.left = PrintfFlagLeft::ZeroPadded; },
			[_, ..] => break,
			[] => return Err(("printf flags or width or kind", None::<u8>).into()),
		}
	}

	let width_and_precision =
		if let b'1'..=b'9' = rest.first().ok_or(("printf width or kind", None::<u8>))? {
			let end = rest.iter().position(|&b| !b.is_ascii_digit()).unwrap_or(rest.len());
			// TODO: Use split_at_unchecked when that is stabilized
			let (s, rest_) = unsafe { (rest.get_unchecked(..end), rest.get_unchecked(end..)) };
			*rest = rest_;

			let width =
				std::str::from_utf8(s)
				.map_err(|_| ("printf width", Some(s)))?
				.parse()
				.map_err(|_| ("printf width", Some(s)))?;

			if split_start(rest, b".") {
				let end = rest.iter().position(|&b| !b.is_ascii_digit()).unwrap_or(rest.len());
				// TODO: Use split_at_unchecked when that is stabilized
				let (s, rest_) = unsafe { (rest.get_unchecked(..end), rest.get_unchecked(end..)) };
				*rest = rest_;

				let precision =
					std::str::from_utf8(s)
					.map_err(|_| ("printf precision", Some(s)))?
					.parse()
					.map_err(|_| ("printf precision", Some(s)))?;
				Some((width, Some(precision)))
			}
			else {
				Some((width, None))
			}
		}
		else {
			None
		};

	let kind = match split_first(rest) {
		Some(b'd') => PrintfKind::Decimal,
		Some(b'o') => PrintfKind::Octal,
		Some(b's') => PrintfKind::String,
		Some(b'x') => PrintfKind::LowerHex,
		Some(b'X') => PrintfKind::UpperHex,
		b => return Err(("printf kind", b).into()),
	};

	Ok(Expr::Printf {
		flags,
		kind,
		width_and_precision,
	})
}

fn split_first(rest: &mut &[u8]) -> Option<u8> {
	match rest {
		[b, rest_ @ ..] => {
			*rest = rest_;
			Some(*b)
		},
		[] => None,
	}
}

fn split_start(rest: &mut &[u8], start: &[u8]) -> bool {
	if rest.starts_with(start) {
		*rest = unsafe { rest.get_unchecked((start.len())..) };
		true
	}
	else {
		false
	}
}

#[allow(clippy::module_name_repetitions)] // Parent module is private
#[derive(Debug)]
pub struct ParseError {
	pub(crate) expected: &'static str,
	pub(crate) actual: Option<String>,
	pos: Option<usize>,
}

impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let ParseError { expected, actual, pos } = self;
		let pos = pos.expect("populated by parse()");

		if let Some(actual) = actual {
			write!(f, "expected {expected} but got {actual} at pos {pos}")
		}
		else {
			write!(f, "expected {expected} but got end-of-input")
		}
	}
}

impl std::error::Error for ParseError {}

impl From<(&'static str, Option<u8>)> for ParseError {
	fn from((expected, actual): (&'static str, Option<u8>)) -> Self {
		ParseError {
			expected,
			actual: actual.map(|actual| actual.escape_ascii().to_string()),
			pos: None,
		}
	}
}

impl From<(&'static str, Option<&u8>)> for ParseError {
	fn from((expected, actual): (&'static str, Option<&u8>)) -> Self {
		(expected, actual.copied()).into()
	}
}

impl From<(&'static str, Option<&[u8]>)> for ParseError {
	fn from((expected, actual): (&'static str, Option<&[u8]>)) -> Self {
		ParseError {
			expected,
			actual: actual.map(|actual| actual.escape_ascii().to_string()),
			pos: None,
		}
	}
}
