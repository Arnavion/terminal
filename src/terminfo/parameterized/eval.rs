use super::{Cmp, Expr, Math, PrintfFlagLeft, PrintfFlagSign, PrintfKind};

// Undocumented detail: Integer parameters are C `long`s.
//
// TODO:
// - params and stack can be either integers or strings.
// - static and dynamic params.
pub fn eval(expr: &[Expr], params: &mut [i32]) -> Result<Vec<u8>, EvalError> {
	let mut result = vec![];
	let mut stack = vec![];
	eval_inner(expr, params, &mut stack, &mut result)?;
	Ok(result)
}

fn eval_inner(expr: &[Expr], params: &mut [i32], stack: &mut Vec<i32>, result: &mut Vec<u8>) -> Result<(), EvalError> {
	for expr in expr {
		match expr {
			Expr::Cmp(c) => match *c {
				Cmp::Equal => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push((a == b).into());
				},
				Cmp::Greater => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push((a > b).into());
				},
				Cmp::Less => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push((a < b).into());
				},
			},

			Expr::If(cond, t, e) => {
				eval_inner(cond, params, stack, result)?;
				// Undocumented detail: %? evaluates truthiness using C `!`, ie Rust `!= 0`
				if stack.pop().ok_or(EvalError::PopFromEmptyStack)? == 0 {
					eval_inner(e, params, stack, result)?;
				}
				else {
					eval_inner(t, params, stack, result)?;
				}
			},

			Expr::IncrementFirstTwoIntegerParams => {
				*params.get_mut(0).ok_or(EvalError::MissingParam(0))? += 1;
				*params.get_mut(1).ok_or(EvalError::MissingParam(1))? += 1;
			},

			Expr::Integer(i) => stack.push(*i),

			Expr::Literal(b) => result.push(*b),

			Expr::LoadVariable(_b) => todo!("variable storage"),

			Expr::Math(m) => match *m {
				Math::Add => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a.wrapping_add(b));
				},
				Math::And => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push((a != 0 && b != 0).into());
				},
				Math::BitAnd => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a & b);
				},
				Math::BitNot => {
					let n = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(!n);
				},
				Math::BitOr => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a | b);
				},
				Math::BitXor => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a ^ b);
				},
				Math::Div => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a.checked_div(b).ok_or(EvalError::DivideByZero)?);
				},
				Math::Mul => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a.wrapping_mul(b));
				},
				Math::Not => {
					let n = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push((n == 0).into());
				},
				Math::Or => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push((a != 0 || b != 0).into());
				},
				Math::Rem => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a.checked_rem(b).ok_or(EvalError::DivideByZero)?);
				},
				Math::Sub => {
					let a = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					let b = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
					stack.push(a.wrapping_sub(b));
				},
			},

			Expr::Param(p) => stack.push(*params.get(*p).ok_or(EvalError::MissingParam(*p))?),

			Expr::Printf { flags, kind, width_and_precision } => {
				use std::io::Write;

				if flags.alternate_form || flags.left != PrintfFlagLeft::Default || flags.sign != PrintfFlagSign::Default || width_and_precision.is_some() {
					todo!();
				}

				let value = stack.pop().ok_or(EvalError::PopFromEmptyStack)?;
				match kind {
					PrintfKind::Char => {
						todo!("strings in stack");
					},

					PrintfKind::Decimal => {
						write!(result, "{}", value).unwrap();
					},

					PrintfKind::LowerHex => {
						write!(result, "{:x}", value).unwrap();
					},

					PrintfKind::Octal => {
						write!(result, "{:o}", value).unwrap();
					},

					PrintfKind::String => {
						todo!("strings in stack");
					},

					PrintfKind::UpperHex => {
						write!(result, "{:X}", value).unwrap();
					},
				}
			},

			Expr::StoreVariable(_b) => todo!("variable storage"),

			Expr::Strlen => {
				todo!("strings in stack");
			},
		}
	}

	Ok(())
}

#[allow(clippy::module_name_repetitions)] // Parent module is private
#[derive(Debug)]
pub enum EvalError {
	DivideByZero,
	PopFromEmptyStack,
	MissingParam(usize),
}

impl std::fmt::Display for EvalError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			EvalError::DivideByZero => f.write_str("divide by zero"),
			EvalError::PopFromEmptyStack => f.write_str("attempted to pop from empty stack"),
			EvalError::MissingParam(p) => write!(f, "attempted to read parameter {p} that was not given"),
		}
	}
}

impl std::error::Error for EvalError {}
