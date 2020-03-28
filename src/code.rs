use custom_error::custom_error;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display, Formatter};

custom_error! {
    pub BytecodeError

    InvalidOpcode{op: u8} = "opcode {op} undefined"
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    Constant,
    Maximum,
}

impl Opcode {
    pub fn operand_widths(&self) -> Option<&'static [usize]> {
        match self {
            Self::Constant => Some(&[2]),
            _ => None,
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TryFrom<u8> for Opcode {
    type Error = BytecodeError;

    fn try_from(op: u8) -> Result<Self, Self::Error> {
        if op >= (Opcode::Constant as u8) && op < (Opcode::Maximum as u8) {
            // We know that it's a valid Opcode here so we can transmute
            Ok(unsafe { std::mem::transmute(op) })
        } else {
            Err(BytecodeError::InvalidOpcode { op })
        }
    }
}

pub fn lookup(op: u8) -> Result<Opcode, BytecodeError> {
    op.try_into()
}

pub fn make(op: Opcode, operands: &[isize]) -> Option<Vec<u8>> {
    let widths = op.operand_widths()?;

    let mut instruction = vec![op as u8];

    for (width, operand) in widths.iter().zip(operands.iter()) {
        match *width {
            2 => instruction.extend_from_slice(&(*operand as u16).to_be_bytes()),
            _ => return None,
        }
    }

    Some(instruction)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_make() {
        let cases = vec![(
            Opcode::Constant,
            &[65534],
            &[Opcode::Constant as u8, 255, 254],
        )];

        for (opcode, operands, result) in cases.into_iter() {
            let instruction = make(opcode, operands).unwrap();

            assert_eq!(result.len(), instruction.len());

            for (expected, actual) in result.iter().zip(instruction.into_iter()) {
                assert_eq!(*expected, actual);
            }
        }
    }
}
