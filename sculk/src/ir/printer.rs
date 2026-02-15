//! IR pretty-printing for debugging

use super::*;
use std::fmt;

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "module {} {{", self.name)?;

        for constant in &self.constants {
            writeln!(
                f,
                "  const {}: {:?} = {:?}",
                constant.name, constant.ty, constant.value
            )?;
        }

        if !self.constants.is_empty() && !self.functions.is_empty() {
            writeln!(f)?;
        }

        for func in &self.functions {
            write!(f, "{}", func)?;
        }

        writeln!(f, "}}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "  fn {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {:?}", param.name, param.ty)?;
        }
        writeln!(f, ") -> {:?} {{", self.return_type)?;

        for block in &self.blocks {
            writeln!(f, "    {}:", block.label)?;
            for inst in &block.instructions {
                writeln!(f, "      {:?}", inst)?;
            }
            writeln!(f, "      {:?}", block.terminator)?;
        }

        writeln!(f, "  }}")?;
        Ok(())
    }
}

/// Print IR to stdout for debugging
pub fn print_ir(module: &Module) {
    println!("{}", module);
}
