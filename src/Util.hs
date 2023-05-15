{-# LANGUAGE DataKinds #-}
module Util where

import Data.Word (Word32)
import qualified What4.Interface as IF
import qualified Data.BitVector.Sized as BV

-- Create a symbolic bitvector from a given 'Word32'.
mkSymWord32 :: IF.IsExprBuilder sym => sym -> Word32 -> IO (IF.SymBV sym 32)
mkSymWord32 sym = IF.bvLit sym (IF.knownRepr :: BV.NatRepr 32) . BV.word32

-- Convert a predicate to a bitvector representing a truth/false
-- value as defined for branch/comparision instructions in RV32.
fromPred :: IF.IsExprBuilder sym => sym -> IF.Pred sym -> IO (IF.SymBV sym 32)
fromPred sym cond = do
  trueBV  <- mkSymWord32 sym 1
  falseBV <- mkSymWord32 sym 0
  IF.bvIte sym cond trueBV falseBV

-- Return a new predicate which checks if a bitvector expression,
-- created using 'fromPred' corresponds to a truth value.
isTrue :: IF.IsExprBuilder sym => sym -> IF.SymBV sym 32 -> IO (IF.Pred sym)
isTrue sym bv = mkSymWord32 sym 1 >>= IF.bvEq sym bv
