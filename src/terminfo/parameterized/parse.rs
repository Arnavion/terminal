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
	match rest.split_first() {
		Some((b'!', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::Not)) },

		Some((b'%', rest_)) => { *rest = rest_; Ok(Expr::Literal(b'%')) },

		Some((b'&', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::BitAnd)) },

		// TODO: char constant
		// Some((b'\'', rest_)) => { *rest = rest_; },

		Some((b'*', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::Mul)) },

		Some((b'+', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::Add)) },

		Some((b'-', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::Sub)) },

		Some((b'/', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::Div)) },

		Some((b'<', rest_)) => { *rest = rest_; Ok(Expr::Cmp(Cmp::Less)) },

		Some((b'=', rest_)) => { *rest = rest_; Ok(Expr::Cmp(Cmp::Equal)) },

		Some((b'>', rest_)) => { *rest = rest_; Ok(Expr::Cmp(Cmp::Greater)) },

		Some((b'?', rest_)) => {
			*rest = rest_;

			let cond = parse_expr_until(rest, &[b"%t"])?;
			if !split_start(rest, b"%t") {
				return Err(ParseError { expected: r#"b"%t""#, actual: rest.first().map(|b| b.escape_ascii().to_string()), pos: None });
			}

			parse_if(rest, cond)
		},

		Some((b'A', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::And)) },

		Some((b'O', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::Or)) },

		Some((b'P', rest_)) => {
			*rest = rest_;
			match split_first(rest) {
				Some(b @ (b'A'..=b'Z' | b'a'..=b'z')) => Ok(Expr::StoreVariable(b)),
				b => Err(ParseError { expected: "valid % directive", actual: b.map(|b| b.escape_ascii().to_string()), pos: None }),
			}
		},

		Some((b'^', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::BitXor)) },

		Some((b'c', rest_)) => {
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

		Some((b'g', rest_)) => {
			*rest = rest_;
			match split_first(rest) {
				Some(b @ (b'A'..=b'Z' | b'a'..=b'z')) => Ok(Expr::LoadVariable(b)),
				b => Err(ParseError { expected: "valid % directive", actual: b.map(|b| b.escape_ascii().to_string()), pos: None }),
			}
		},

		Some((b'i', rest_)) => { *rest = rest_; Ok(Expr::IncrementFirstTwoIntegerParams) },

		Some((b'l', rest_)) => { *rest = rest_; Ok(Expr::Strlen) },

		Some((b'm', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::Rem)) },

		Some((b'p', rest_)) => {
			*rest = rest_;
			let param_num = parse_param_num(rest)?;
			Ok(Expr::Param(param_num))
		},

		Some((b'{', rest_)) => {
			*rest = rest_;
			let integer = parse_integer(rest)?;
			Ok(Expr::Integer(integer))
		},

		Some((b'|', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::BitOr)) },

		Some((b'~', rest_)) => { *rest = rest_; Ok(Expr::Math(Math::BitNot)) },

		Some(_) => parse_printf(rest),

		None => Err(ParseError { expected: "valid % directive", actual: None, pos: None }),
	}
}

fn parse_param_num(rest: &mut &[u8]) -> Result<usize, ParseError> {
	match split_first(rest) {
		Some(b @ b'1'..=b'9') => Ok((b - b'1').into()),
		b => Err(ParseError { expected: "b'1'..=b'9'", actual: b.map(|b| b.escape_ascii().to_string()), pos: None }),
	}
}

fn parse_integer(rest: &mut &[u8]) -> Result<i32, ParseError> {
	let mut end = 0;
	for &b in *rest {
		if b == b'}' {
			break;
		}

		end += 1;
	}
	if end == 0 {
		return Err(ParseError { expected: "integer", actual: rest.first().map(|b| b.escape_ascii().to_string()), pos: None });
	}

	// TODO: Use split_at_unchecked when that is stabilized
	let (s, rest_) = rest.split_at(end);
	*rest = rest_;

	if !split_start(rest, b"}") {
		return Err(ParseError { expected: "b'}'", actual: rest.get(end).map(|b| b.escape_ascii().to_string()), pos: None });
	}

	let i =
		std::str::from_utf8(s)
		.map_err(|_| ParseError { expected: "integer", actual: Some(s.escape_ascii().to_string()), pos: None })?
		.parse()
		.map_err(|_| ParseError { expected: "integer", actual: Some(s.escape_ascii().to_string()), pos: None })?;
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
				return Err(ParseError { expected: r#"b"%t" | b"%;""#, actual: rest.first().map(|b| b.escape_ascii().to_string()), pos: None });
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
			return Err(ParseError { expected: r#"b"%e" | b"%;""#, actual: rest.first().map(|b| b.escape_ascii().to_string()), pos: None });
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
		match rest.split_first().ok_or(ParseError { expected: "printf flags or width or kind", actual: None, pos: None })? {
			(b' ', rest_) => { *rest = rest_; flags.sign = PrintfFlagSign::Space; },
			(b'#', rest_) => { *rest = rest_; flags.alternate_form = true; },
			(b'+', rest_) => { *rest = rest_; flags.sign = PrintfFlagSign::Plus; },
			(b'-', rest_) => { *rest = rest_; flags.left = PrintfFlagLeft::Aligned; },
			(b'0', rest_) => { *rest = rest_; flags.left = PrintfFlagLeft::ZeroPadded; },
			_ => break,
		}
	}

	let mut width_and_precision = None;

	if let b'1'..=b'9' = rest.first().ok_or(ParseError { expected: "printf width or kind", actual: None, pos: None })? {
		let mut end = 0;
		for &b in *rest {
			if let b'0'..=b'9' = b {
				end += 1;
			}
			else {
				break;
			}
		}

		// TODO: Use split_at_unchecked when that is stabilized
		let (s, rest_) = rest.split_at(end);
		*rest = rest_;

		let width =
			std::str::from_utf8(s)
			.map_err(|_| ParseError { expected: "printf width", actual: Some(s.escape_ascii().to_string()), pos: None })?
			.parse()
			.map_err(|_| ParseError { expected: "printf width", actual: Some(s.escape_ascii().to_string()), pos: None })?;

		if split_start(rest, b".") {
			let mut end = 0;
			for &b in *rest {
				if let b'0'..=b'9' = b {
					end += 1;
				}
				else {
					break;
				}
			}

			if end == 0 {
				return Err(ParseError { expected: "printf precision", actual: rest.first().map(|b| b.escape_ascii().to_string()), pos: None });
			}

			// TODO: Use split_at_unchecked when that is stabilized
			let (s, rest_) = rest.split_at(end);
			*rest = rest_;

			let precision =
				std::str::from_utf8(s)
				.map_err(|_| ParseError { expected: "printf precision", actual: Some(s.escape_ascii().to_string()), pos: None })?
				.parse()
				.map_err(|_| ParseError { expected: "printf precision", actual: Some(s.escape_ascii().to_string()), pos: None })?;
			width_and_precision = Some((width, Some(precision)));
		}
		else {
			width_and_precision = Some((width, None));
		}
	}

	let kind = match split_first(rest) {
		Some(b'd') => PrintfKind::Decimal,
		Some(b'o') => PrintfKind::Octal,
		Some(b's') => PrintfKind::String,
		Some(b'x') => PrintfKind::LowerHex,
		Some(b'X') => PrintfKind::UpperHex,
		b => return Err(ParseError { expected: "printf kind", actual: b.map(|b| b.escape_ascii().to_string()), pos: None }),
	};

	Ok(Expr::Printf {
		flags,
		kind,
		width_and_precision,
	})
}

fn split_first(rest: &mut &[u8]) -> Option<u8> {
	match rest.split_first() {
		Some((&b, rest_)) => {
			*rest = rest_;
			Some(b)
		},
		None => None,
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