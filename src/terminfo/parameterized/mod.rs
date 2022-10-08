/// Parameterized strings parser and evaulator

mod eval;
pub use eval::{eval, EvalError};

mod parse;
pub use parse::{parse, ParseError};

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
	Cmp(Cmp),
	If(Vec<Expr>, Vec<Expr>, Vec<Expr>),
	IncrementFirstTwoIntegerParams,
	Integer(i32),
	Literal(u8),
	LoadVariable(u8),
	Math(Math),
	Param(usize), // zero-indexed
	Printf {
		flags: PrintfFlags,
		kind: PrintfKind,
		width_and_precision: Option<(usize, Option<usize>)>,
	},
	StoreVariable(u8),
	Strlen,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Cmp {
	Equal,
	Greater,
	Less,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Math {
	Add,
	And,
	BitAnd,
	BitNot,
	BitOr,
	BitXor,
	Div,
	Mul,
	Not,
	Or,
	Rem,
	Sub,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PrintfFlags {
	alternate_form: bool,
	left: PrintfFlagLeft,
	sign: PrintfFlagSign,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrintfFlagLeft {
	Default,
	Aligned,
	ZeroPadded,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrintfFlagSign {
	Default,
	Plus,
	Space,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrintfKind {
	Char,
	Decimal,
	LowerHex,
	Octal,
	String,
	UpperHex,
}

#[cfg(test)]
mod tests {
	use super::{Cmp, Expr, Math, PrintfKind, PrintfFlagLeft, PrintfFlags, PrintfFlagSign, eval, parse};

	// From foot-extra
	#[test]
	fn sync() {
		const INPUT: &[u8] = b"\x1b[?2026%?%p1%{1}%-%tl%eh";

		let expr = parse(INPUT).unwrap();
		assert_eq!(expr, vec![
			Expr::Literal(b'\x1b'),
			Expr::Literal(b'['),
			Expr::Literal(b'?'),
			Expr::Literal(b'2'),
			Expr::Literal(b'0'),
			Expr::Literal(b'2'),
			Expr::Literal(b'6'),
			Expr::If(
				vec![Expr::Param(0), Expr::Integer(1), Expr::Math(Math::Sub)],
				vec![Expr::Literal(b'l')],
				vec![Expr::Literal(b'h')],
			),
		]);

		let mut s = vec![];
		eval(&expr, &mut [1], &mut s).unwrap();
		assert_eq!(s, b"\x1b[?2026h");
		let mut s = vec![];
		eval(&expr, &mut [2], &mut s).unwrap();
		assert_eq!(s, b"\x1b[?2026l");
	}

	// From Eterm-256color
	#[test]
	fn else_if() {
		const INPUT: &[u8] = b"\x1b[%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m";
		let expr = parse(INPUT).unwrap();
		assert_eq!(expr, vec![
			Expr::Literal(b'\x1b'),
			Expr::Literal(b'['),
			Expr::If(
				vec![Expr::Param(0), Expr::Integer(8), Expr::Cmp(Cmp::Less)],
				vec![
					Expr::Literal(b'3'),
					Expr::Param(0),
					Expr::Printf {
						flags: PrintfFlags {
							alternate_form: false,
							left: PrintfFlagLeft::Default,
							sign: PrintfFlagSign::Default,
						},
						kind: PrintfKind::Decimal,
						width_and_precision: None,
					},
				],
				vec![Expr::If(
					vec![Expr::Param(0), Expr::Integer(16), Expr::Cmp(Cmp::Less)],
					vec![
						Expr::Literal(b'9'),
						Expr::Param(0),
						Expr::Integer(8),
						Expr::Math(Math::Sub),
						Expr::Printf {
							flags: PrintfFlags {
								alternate_form: false,
								left: PrintfFlagLeft::Default,
								sign: PrintfFlagSign::Default,
							},
							kind: PrintfKind::Decimal,
							width_and_precision: None,
						},
					],
					vec![
						Expr::Literal(b'3'),
						Expr::Literal(b'8'),
						Expr::Literal(b';'),
						Expr::Literal(b'5'),
						Expr::Literal(b';'),
						Expr::Param(0),
						Expr::Printf {
							flags: PrintfFlags {
								alternate_form: false,
								left: PrintfFlagLeft::Default,
								sign: PrintfFlagSign::Default,
							},
							kind: PrintfKind::Decimal,
							width_and_precision: None,
						},
					],
				)],
			),
			Expr::Literal(b'm'),
		]);
	}

	// From ansi
	#[test]
	fn printf_c() {
		const INPUT: &[u8] = b"%p1%c\x1b[%p2%{1}%-%db";
		let expr = parse(INPUT).unwrap();
		assert_eq!(expr, vec![
			Expr::Param(0),
			Expr::Printf {
				flags: PrintfFlags {
					alternate_form: false,
					left: PrintfFlagLeft::Default,
					sign: PrintfFlagSign::Default,
				},
				kind: PrintfKind::Char,
				width_and_precision: None,
			},
			Expr::Literal(b'\x1b'),
			Expr::Literal(b'['),
			Expr::Param(1),
			Expr::Integer(1),
			Expr::Math(Math::Sub),
			Expr::Printf {
				flags: PrintfFlags {
					alternate_form: false,
					left: PrintfFlagLeft::Default,
					sign: PrintfFlagSign::Default,
				},
				kind: PrintfKind::Decimal,
				width_and_precision: None,
			},
			Expr::Literal(b'b'),
		]);
	}
}
