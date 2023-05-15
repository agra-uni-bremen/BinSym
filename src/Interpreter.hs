{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Interpreter where

import Util

import GHC.TypeLits
import qualified What4.Interface as IF
import qualified LibRISCV.Spec.Expr as E

-- Map a binary operation in the LibRISCV expression language to a What4 operation.
binOp :: (IF.IsExprBuilder sym)
  => sym
  -> E.Expr (IF.SymBV sym 32)
  -> E.Expr (IF.SymBV sym 32)
  -> (sym -> IF.SymBV sym 32 -> IF.SymBV sym 32 -> IO (IF.SymBV sym 32))
  -> IO (IF.SymBV sym 32)
binOp sym e1 e2 op = do
  bv1 <- evalE sym e1
  bv2 <- evalE sym e2
  op sym bv1 bv2

evalE :: IF.IsExprBuilder sym
  => sym
  -> E.Expr (IF.SymBV sym 32)
  -> IO (IF.SymBV sym 32)
evalE _ (E.FromImm e) = pure e
evalE sym (E.FromUInt v) = mkSymWord32 sym v
evalE sym (E.Add e1 e2) = binOp e1 e2 IF.bvAdd
evalE _ _ = error "expression language operation not implemented"
