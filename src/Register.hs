module Register where

import Util

import qualified Z3.Monad as Z3

-- A register file for manipulating general purpose registers and the PC.
data RegisterFile = RegisterFile
                      Z3.AST -- Program Counter (32-bit BV)
                      Z3.AST -- General purpose registers (Array)

-- Create a new register file, represented internally as a Z3 Array.
mkRegFile :: Z3.MonadZ3 z3 => z3 RegisterFile
mkRegFile = do
  indexSort <- Z3.mkBvSort 32
  zeroValue <- mkSymWord32 0

  array <- Z3.mkConstArray indexSort zeroValue
  pure $ RegisterFile zeroValue array

------------------------------------------------------------------------

-- Read a general purpose register from the register file.
-- Reads of the zero register always return zero.
-- The register index needs to be a given as 32-bit Z3 bit vector.
readRegister :: Z3.MonadZ3 z3 => RegisterFile -> Z3.AST -> z3 Z3.AST
readRegister (RegisterFile _ regs) index = do
  zero <- mkSymWord32 0
  cond <- Z3.mkEq index zero

  regRead <- Z3.mkSelect regs index
  Z3.mkIte cond zero regRead

-- Return a new register file where the given register was written.
-- The register index needs to be a given as 32-bit Z3 bit vector.
writeRegister :: Z3.MonadZ3 z3 => RegisterFile -> Z3.AST -> Z3.AST -> z3 RegisterFile
writeRegister (RegisterFile pc regs) index value = do
  newArray <- Z3.mkStore regs index value
  pure $ RegisterFile pc newArray

-- Read the program counter values as a 32-bit Z3 bit vector.
readPC :: RegisterFile -> Z3.AST
readPC (RegisterFile pc _) = pc

-- Return a new register file with a modified program counter.
writePC :: RegisterFile -> Z3.AST -> RegisterFile
writePC (RegisterFile _ regs) newPC = RegisterFile newPC regs
