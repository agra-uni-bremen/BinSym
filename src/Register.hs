module Register where

import Util

import qualified Z3.Monad as Z3

data RegisterFile = RegisterFile
                      Z3.AST -- Program Counter (32-bit BV)
                      Z3.AST -- General purpose registers (Array)

mkRegFile :: Z3.MonadZ3 z3 => z3 RegisterFile
mkRegFile = do
  indexSort <- Z3.mkBvSort 32
  zeroValue <- mkSymWord32 0

  array <- Z3.mkConstArray indexSort zeroValue
  pure $ RegisterFile zeroValue array

------------------------------------------------------------------------

readRegister :: Z3.MonadZ3 z3 => RegisterFile -> Z3.AST -> z3 Z3.AST
readRegister (RegisterFile _ regs) index = do
  zero <- mkSymWord32 0
  cond <- Z3.mkEq index zero

  regRead <- Z3.mkSelect regs index
  Z3.mkIte cond zero regRead

writeRegister :: Z3.MonadZ3 z3 => RegisterFile -> Z3.AST -> Z3.AST -> z3 RegisterFile
writeRegister (RegisterFile pc regs) index value = do
  newArray <- Z3.mkStore regs index value
  pure $ RegisterFile pc newArray
