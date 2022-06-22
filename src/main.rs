use std::error::Error;
use std::{env, fs};

use std::collections::hash_map::HashMap;

use anyhow::{anyhow, Result};
use regex::Regex;

#[derive(Debug, Clone, Copy)]
pub enum Destination {
    None = 0,
    M,
    D,
    DM,
    A,
    AM,
    AD,
    ADM,
}

impl TryFrom<&str> for Destination {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let m = match s {
            "" => Destination::None,
            "M" => Destination::M,
            "D" => Destination::D,
            "MD" => Destination::DM,
            "A" => Destination::A,
            "AM" => Destination::AM,
            "AD" => Destination::AD,
            "AMD" => Destination::ADM,
            _ => return Err(anyhow!("no matching destination"))
        };
        Ok(m)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Jump {
    None = 0,
    JGT,
    JEQ,
    JGE,
    JLT,
    JNE,
    JLE,
    JMP,
}

impl TryFrom<&str> for Jump {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let m = match s {
            "" => Jump::None,
            "JGT" => Jump::JGT,
            "JEQ" => Jump::JEQ,
            "JGE" => Jump::JGE,
            "JLT" => Jump::JLT,
            "JNE" => Jump::JNE,
            "JLE" => Jump::JLE,
            "JMP" => Jump::JMP,
            _ => return Err(anyhow!("no matching jump")),
        };
        Ok(m)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Computation {
    Zero = 0b0101010,
    One = 0b0111111,
    NegativeOne = 0b0111010,
    D = 0b0001100,
    A = 0b0110000,
    M = 0b1110000,
    NotD = 0b0001101,
    NotA = 0b0110001,
    NotM = 0b1110001,
    NegativeD = 0b0001111,
    NegativeA = 0b0110011,
    NegativeM = 0b1110011,
    DPlus1 = 0b0011111,
    APlus1 = 0b0110111,
    MPlus1 = 0b1110111,
    DMinus1 = 0b0001110,
    AMinus1 = 0b0110010,
    MMinus1 = 0b1110010,
    DPlusA = 0b0000010,
    DPlusM = 0b1000010,
    DMinusA = 0b0010011,
    DMinusM = 0b1010011,
    AMinusD = 0b0000111,
    MMinusD = 0b1000111,
    DAndA = 0b0000000,
    DAndM = 0b1000000,
    DOrA = 0b0010101,
    DOrM = 0b1010101,
}

impl TryFrom<&str> for Computation {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let m = match s {
            "0" => Computation::Zero,
            "1" => Computation::One,
            "-1" => Computation::NegativeOne,
            "D" => Computation::D,
            "A" => Computation::A,
            "M" => Computation::M,
            "!D" => Computation::NotD,
            "!A" => Computation::NotA,
            "!M" => Computation::NotM,
            "-D" => Computation::NegativeD,
            "-A" => Computation::NegativeA,
            "-M" => Computation::NegativeM,
            "D+1" => Computation::DPlus1,
            "A+1" => Computation::APlus1,
            "M+1" => Computation::MPlus1,
            "D-1" => Computation::DMinus1,
            "A-1" => Computation::AMinus1,
            "M-1" => Computation::MMinus1,
            "D+A" => Computation::DPlusA,
            "D+M" => Computation::DPlusM,
            "D-A" => Computation::DMinusA,
            "D-M" => Computation::DMinusM,
            "A-D" => Computation::AMinusD,
            "M-D" => Computation::MMinusD,
            "D&A" => Computation::DAndA,
            "D&M" => Computation::DAndM,
            "D|A" => Computation::DOrA,
            "D|M" => Computation::DOrM,
            _ => return Err(anyhow!("no matching computation")),
        };
        Ok(m)
    }
}


#[derive(Debug, Clone)]
pub struct C {
    dest: Destination,
    computation: Computation,
    jump: Jump,
}

impl C {
    fn new(dest: Destination, computation: Computation, jump: Jump) -> C {
        C {
            dest,
            computation,
            jump,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    A(u16),
    C(C),
}

impl From<&Instruction> for u16 {
    fn from(instruction: &Instruction) -> Self {
        match instruction {
            Instruction::A(value) => *value,
            Instruction::C(c) => {
                0b1110000000000000
                    | (c.computation as u16) << 6
                    | (c.dest as u16) << 3
                    | (c.jump as u16)
            }
        }
    }
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        format!("{:016b}", u16::from(self))
    }
}

fn remove_whitespace(s: &str) -> String {
    s.chars().filter(|c| !c.is_whitespace()).collect()
}

struct Parser {
    symbols: HashMap<String, u16>,
    variable: u16,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            symbols: HashMap::new(),
            variable: 16,
        }
    }

    fn symbol(&mut self, symbol: &str) -> u16 {
        let predefined = match symbol {
            "SP" => Some(0),
            "LCL" => Some(1),
            "ARG" => Some(2),
            "THIS" => Some(3),
            "THAT" => Some(4),
            "R0" => Some(0),
            "R1" => Some(1),
            "R2" => Some(2),
            "R3" => Some(3),
            "R4" => Some(4),
            "R5" => Some(5),
            "R6" => Some(6),
            "R7" => Some(7),
            "R8" => Some(8),
            "R9" => Some(9),
            "R10" => Some(10),
            "R11" => Some(11),
            "R12" => Some(12),
            "R13" => Some(13),
            "R14" => Some(14),
            "R15" => Some(15),
            "SCREEN" => Some(16384),
            "KBD" => Some(24576),
            _ => None,
        };
        if let Some(v) = predefined {
            v
        } else if let Some(value) = self.symbols.get(symbol) {
            *value
        } else {
            *self.symbols.entry(symbol.to_string()).or_insert_with(|| {
                self.variable += 1;
                self.variable - 1
            })
        }
    }

    fn build_symbol_table(&mut self, lines: &Vec<String>) -> Result<()> {
        // TODO: Would be nice to not use regex here
        let symbol = Regex::new(r"\(([^\d].*)\)")?;
        let mut line_no = 0;
        for line in lines {
            if let Some(cap) = symbol.captures(line) {
                self.symbols.insert(cap.get(1).unwrap().as_str().to_string(), line_no);
            } else {
                line_no += 1;
            }
        }
        Ok(())
    }

    fn parse_line(&mut self, line: &str) -> Result<Option<Instruction>> {
        if line.starts_with("(") {
            Ok(None)
        } else if line.starts_with("@") {
            let chars: Vec<_> = line.chars().collect();
            let is_symbol = !chars.get(1).unwrap().is_digit(10);
            let rest = line[1..].to_string();

            if is_symbol {
                Ok(Some(Instruction::A(self.symbol(&rest))))
            } else {
                Ok(Some(Instruction::A(rest.parse()?)))
            }
        } else {
            if let Some((dest, rest)) = line.split_once("=") {
                let dest = Destination::try_from(dest)?;
                if let Some((comp, jump)) = rest.split_once(";") {
                    let jump = Jump::try_from(jump)?;
                    let comp = Computation::try_from(comp)?;
                    Ok(Some(Instruction::C(C::new(dest, comp, jump))))
                } else {
                    let comp = Computation::try_from(rest)?;
                    Ok(Some(Instruction::C(C::new(dest, comp, Jump::None))))
                }
            } else {
                if let Some((comp, jump)) = line.split_once(";") {
                    let jump = Jump::try_from(jump)?;
                    let comp = Computation::try_from(comp)?;
                    Ok(Some(Instruction::C(C::new(Destination::None, comp, jump))))
                } else {
                    let comp = Computation::try_from(line)?;
                    Ok(Some(Instruction::C(C::new(Destination::None, comp, Jump::None))))
                }
            }
        }
    }

    fn parse(program: &str) -> Result<Vec<Instruction>> {
        let mut parser = Parser::new();
        let lines = program
            .lines()
            .filter_map(|l| {
                let line = remove_whitespace(l);

                if line.is_empty() {
                    None
                } else if let Some((instruction, _comment)) = line.split_once("//") {
                    if instruction.is_empty() {
                        None
                    } else {
                        Some(instruction.to_string())
                    }
                } else {
                    Some(line)
                }
            })
            .collect();


        parser.build_symbol_table(&lines)?;

        let mut instructions = vec![];

        for line in lines {
            match parser.parse_line(&line) {
                Ok(Some(instruction)) => {
                    instructions.push(instruction);
                }
                Err(err) => return Err(err),
                _ => (),
            }
        }

        Ok(instructions)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).ok_or(anyhow!("Missing filename"))?;
    let content = fs::read_to_string(filename)?;

    let instructions = Parser::parse(&content)?;
    let program = instructions
        .iter()
        .map(|i| i.to_string())
        .collect::<Vec<String>>()
        .join("\n");
    println!("{}", program);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_instruction() {
        let instruction = Instruction::A(2);
        assert_eq!(instruction.to_string(), "0000000000000010");
    }

    #[test]
    fn c_instruction() {
        let instruction = Instruction::C(C::new(Destination::None, Computation::D, Jump::JGT));
        assert_eq!(instruction.to_string(), "1110001100000001");
    }
}
