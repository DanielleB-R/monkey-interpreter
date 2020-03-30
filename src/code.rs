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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Instructions(Vec<u8>);

impl From<Vec<u8>> for Instructions {
    fn from(v: Vec<u8>) -> Self {
        Self(v)
    }
}

impl IntoIterator for Instructions {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Ok(())
    }
}

impl Instructions {
    fn len(&self) -> usize {
        self.0.len()
    }
}

pub fn make(op: Opcode, operands: &[isize]) -> Option<Instructions> {
    let widths = op.operand_widths()?;

    let mut instruction = vec![op as u8];

    for (width, operand) in widths.iter().zip(operands.iter()) {
        match *width {
            2 => instruction.extend_from_slice(&(*operand as u16).to_be_bytes()),
            _ => return None,
        }
    }

    Some(instruction.into())
}

pub fn concat_instructions(data: Vec<Instructions>) -> Instructions {
    data.into_iter().flatten().collect::<Vec<u8>>().into()
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

    #[test]
    fn test_instructions_display() {
        let insts = vec![
            make(Opcode::Constant, &[1]).unwrap(),
            make(Opcode::Constant, &[2]).unwrap(),
            make(Opcode::Constant, &[65535]).unwrap(),
        ];

        let expected = "0000 Constant 1
0003 Constant 2
0006 Constant 65535
";

        assert_eq!(concat_instructions(insts).to_string(), expected);
    }
}
