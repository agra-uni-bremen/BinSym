module Util where

import Data.Word (Word32)
import Control.Exception (assert)

import qualified Z3.Monad as Z3

-- Create a symbolic bitvector from a 'Word32'.
mkSymWord32 :: Z3.MonadZ3 z3 => Word32 -> z3 Z3.AST
mkSymWord32 w = Z3.mkBitvector 32 (fromIntegral w)

-- Convert a predicate to a bitvector representing a truth/false
-- value as defined for branch/compare instructions in RISC-V.
fromBool :: Z3.MonadZ3 z3 => Z3.AST -> z3 Z3.AST
fromBool boolAst = do
  sort <- Z3.getSort boolAst >>= Z3.getSortKind
  assert (sort == Z3.Z3_BOOL_SORT) (fromBool' boolAst)
 where
  fromBool' boolAst = do
    trueBV  <- mkSymWord32 1
    falseBV <- mkSymWord32 0
    Z3.mkIte boolAst trueBV falseBV
