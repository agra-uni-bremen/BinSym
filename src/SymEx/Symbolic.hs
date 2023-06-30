module SymEx.Symbolic (evalE) where

import Control.Exception (assert)
import qualified LibRISCV.Effects.Expressions.Expr as E
import SymEx.Util
import qualified Z3.Monad as Z3

-- Map a binary operation in the LibRISCV expression language to a Z3 operation.
binOp :: (Z3.MonadZ3 z3) => E.Expr Z3.AST -> E.Expr Z3.AST -> (Z3.AST -> Z3.AST -> z3 Z3.AST) -> z3 Z3.AST
binOp e1 e2 op = do
  bv1 <- evalE e1
  bv2 <- evalE e2
  op bv1 bv2

{- ORMOLU_DISABLE -}
evalE :: Z3.MonadZ3 z3 => E.Expr Z3.AST -> z3 Z3.AST
evalE (E.FromImm e)       = pure e
evalE (E.FromInt n v)     = Z3.mkBitvector n v
evalE (E.ZExt n v)        = evalE v >>= Z3.mkZeroExt n
evalE (E.SExt n v)        = evalE v >>= Z3.mkSignExt n
evalE (E.Extract i len e) = evalE e >>= Z3.mkExtract (assert (len >= 1) $ i + (len - 1)) i
evalE (E.Add e1 e2)       = binOp e1 e2 Z3.mkBvadd
evalE (E.Sub e1 e2)       = binOp e1 e2 Z3.mkBvsub
evalE (E.Eq e1 e2)        = binOp e1 e2 Z3.mkEq     >>= fromBool
evalE (E.Slt e1 e2)       = binOp e1 e2 Z3.mkBvslt  >>= fromBool
evalE (E.Sge e1 e2)       = binOp e1 e2 Z3.mkBvsge  >>= fromBool
evalE (E.Ult e1 e2)       = binOp e1 e2 Z3.mkBvult  >>= fromBool
evalE (E.Uge e1 e2)       = binOp e1 e2 Z3.mkBvuge  >>= fromBool
evalE (E.And e1 e2)       = binOp e1 e2 Z3.mkBvand
evalE (E.Or e1 e2)        = binOp e1 e2 Z3.mkBvor
evalE (E.Xor e1 e2)       = binOp e1 e2 Z3.mkBvxor
evalE (E.LShl e1 e2)      = binOp e1 e2 Z3.mkBvshl
evalE (E.LShr e1 e2)      = binOp e1 e2 Z3.mkBvlshr
evalE (E.AShr e1 e2)      = binOp e1 e2 Z3.mkBvashr
evalE (E.Mul e1 e2)       = binOp e1 e2 Z3.mkBvmul
evalE (E.SDiv e1 e2)      = binOp e1 e2 Z3.mkBvsdiv
evalE (E.UDiv e1 e2)      = binOp e1 e2 Z3.mkBvudiv
evalE (E.SRem e1 e2)      = binOp e1 e2 Z3.mkBvsrem
evalE (E.URem e1 e2)      = binOp e1 e2 Z3.mkBvurem
{- ORMOLU_ENABLE -}
