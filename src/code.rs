use custom_error::custom_error;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display, Formatter};
use std::ops::{Index, RangeFrom};

custom_error! {
    pub BytecodeError

    InvalidOpcode{op: u8} = "opcode {op} undefined"
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    Pop,
    Maximum,
}

static NO_ARGS: Option<&'static [usize]> = Some(&[]);

impl Opcode {
    pub fn operand_widths(self) -> Option<&'static [usize]> {
        match self {
            Self::Constant => Some(&[2]),
            Self::Add => NO_ARGS,
            Self::Sub => NO_ARGS,
            Self::Mul => NO_ARGS,
            Self::Div => NO_ARGS,
            Self::Pop => NO_ARGS,
            Self::Maximum => None,
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

impl Index<usize> for Instructions {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Index<RangeFrom<usize>> for Instructions {
    type Output = [u8];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.0[index]
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut i = 0;
        while i < self.0.len() {
            // TODO do this error handling right
            let op = lookup(self.0[i]).unwrap();
            let (operands, read) = read_operands(op, &self.0[(i + 1)..]);
            writeln!(f, "{:04} {}", i, self.fmt_instruction(op, &operands))?;
            i += 1 + read;
        }
        Ok(())
    }
}

impl Instructions {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn fmt_instruction(&self, op: Opcode, operands: &[isize]) -> String {
        let operand_count = op.operand_widths().unwrap().len();
        if operand_count == operands.len() {
            match operand_count {
                0 => format!("{}", op),
                1 => format!("{} {}", op, operands[0]),
                n => format!("ERROR unhandled operand count {}\n", n),
            }
        } else {
            format!(
                "ERROR operand len {} doesn't match defined {}\n",
                operands.len(),
                operand_count
            )
        }
    }

    pub fn append(&mut self, additional: Self) {
        self.0.extend_from_slice(&additional.0);
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

pub fn read_operands(op: Opcode, bytecode: &[u8]) -> (Vec<isize>, usize) {
    let mut operands = vec![];
    let mut offset = 0;

    for width in op.operand_widths().unwrap() {
        match width {
            2 => {
                operands.push(read_u16(&bytecode[offset..]) as isize);
            }
            _ => panic!("not implemented"),
        }

        offset += width;
    }

    (operands, offset)
}

pub fn read_u16(bytecode: &[u8]) -> u16 {
    u16::from_be_bytes([bytecode[0], bytecode[1]])
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_make() {
        let cases = vec![
            (
                Opcode::Constant,
                vec![65534],
                vec![Opcode::Constant as u8, 255, 254],
            ),
            (Opcode::Add, vec![], vec![Opcode::Add as u8]),
        ];

        for (opcode, operands, result) in cases.into_iter() {
            let instruction = make(opcode, &operands).unwrap();

            assert_eq!(result.len(), instruction.len());

            for (expected, actual) in result.into_iter().zip(instruction.into_iter()) {
                assert_eq!(expected, actual);
            }
        }
    }

    #[test]
    fn test_instructions_display() {
        let insts = vec![
            make(Opcode::Add, &[]).unwrap(),
            make(Opcode::Constant, &[2]).unwrap(),
            make(Opcode::Constant, &[65535]).unwrap(),
        ];

        let expected = "0000 Add
0001 Constant 2
0004 Constant 65535
";

        assert_eq!(concat_instructions(insts).to_string(), expected);
    }

    #[test]
    fn test_read_operands() {
        let cases = vec![(Opcode::Constant, &[65535], 2)];

        for (op, operands, bytes_read) in cases {
            let instruction = make(op, operands).unwrap();

            let (operands_read, n) = read_operands(op, &instruction.0[1..]);
            assert_eq!(n, bytes_read);
            assert_eq!(operands_read, operands);
        }
    }
}
