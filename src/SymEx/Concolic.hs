module SymEx.Concolic (Concolic, mkConcrete, evalE) where

import Data.Maybe (isJust, fromMaybe)
import SymEx.Util (mkSymWord32)
import Data.Word (Word32)
import qualified Z3.Monad as Z3
import qualified SymEx.Symbolic as S
import qualified LibRISCV.Spec.Expr as E
import qualified LibRISCV.Machine.Interpreter as I

-- Concolic is a tuple of a concrete value (as represented by
-- LibRISCV.Machine.Interpreter) and an optional symbolic value as an
-- SMT bit-vector (as represented by the Haskell Z3 bindings).
--
-- See also: https://en.wikipedia.org/wiki/Concolic_testing
data Concolic = MkConcolic
                  Word32         -- Concrete value
                  (Maybe Z3.AST) -- Symbolic value (bit-vector)

-- Create a concrete concolic value, i.e. a value without a symbolic part.
mkConcrete :: Word32 -> Concolic
mkConcrete w = MkConcolic w Nothing

-- Create a concolic value with an unconstrained symbolic value.
-- TODO: Requires random number generator (e.g. System.Random).
-- mkSymbolic :: Concolic
-- mkSymbolic = 

------------------------------------------------------------------------

unaryOp :: Z3.MonadZ3 z3 => E.Expr Concolic -> (E.Expr Word32 -> E.Expr Word32) -> (E.Expr Z3.AST -> E.Expr Z3.AST) -> z3 Concolic
unaryOp e fnConc fnSym = do
  (MkConcolic c s) <- evalE e

  let concrete = I.runExpression (fnConc (E.FromImm c))
  symbolic <- case s of
    Just x  -> Just <$> S.evalE (fnSym (E.FromImm x))
    Nothing -> pure Nothing

  pure $ MkConcolic concrete symbolic

binaryOp :: Z3.MonadZ3 z3 => E.Expr Concolic -> E.Expr Concolic -> (E.Expr Word32 -> E.Expr Word32 -> E.Expr Word32) -> (E.Expr Z3.AST -> E.Expr Z3.AST -> E.Expr Z3.AST) -> z3 Concolic
binaryOp e1 e2 fnConc fnSym = do
  (MkConcolic c1 s1) <- evalE e1
  (MkConcolic c2 s2) <- evalE e2

  let concrete = I.runExpression (fnConc (E.FromImm c1) (E.FromImm c2))
  if isJust s1 || isJust s2
    then do
      sym1 <- flip fromMaybe s1 <$> mkSymWord32 c1
      sym2 <- flip fromMaybe s2 <$> mkSymWord32 c2

      symbolic <- Just <$> S.evalE (fnSym (E.FromImm sym1) (E.FromImm sym2))
      pure $ MkConcolic concrete symbolic
    else pure $ MkConcolic concrete Nothing

{- ORMOLU_DISABLE -}
evalE :: Z3.MonadZ3 z3 => E.Expr Concolic -> z3 Concolic
evalE (E.FromImm e)  = pure e
evalE (E.FromUInt v) = pure $ mkConcrete v
evalE (E.ZExtByte e) = unaryOp e E.ZExtByte E.ZExtByte
evalE (E.ZExtHalf e) = unaryOp e E.ZExtHalf E.ZExtHalf
evalE (E.SExtByte e) = unaryOp e E.SExtByte E.SExtByte
evalE (E.SExtHalf e) = unaryOp e E.SExtHalf E.SExtHalf
evalE (E.Add e1 e2)  = binaryOp e1 e2 E.Add E.Add
evalE (E.Sub e1 e2)  = binaryOp e1 e2 E.Sub E.Sub
evalE (E.Eq e1 e2)   = binaryOp e1 e2 E.Eq E.Eq
evalE (E.Slt e1 e2)  = binaryOp e1 e2 E.Slt E.Slt
evalE (E.Sge e1 e2)  = binaryOp e1 e2 E.Sge E.Sge
evalE (E.Ult e1 e2)  = binaryOp e1 e2 E.Ult E.Ult
evalE (E.Uge e1 e2)  = binaryOp e1 e2 E.Uge E.Uge
evalE (E.And e1 e2)  = binaryOp e1 e2 E.And E.And
evalE (E.Or e1 e2)   = binaryOp e1 e2 E.Or E.Or
evalE (E.Xor e1 e2)  = binaryOp e1 e2 E.Xor E.Xor
evalE (E.LShl e1 e2) = binaryOp e1 e2 E.LShl E.LShl
evalE (E.LShr e1 e2) = binaryOp e1 e2 E.LShr E.LShr
evalE (E.AShr e1 e2) = binaryOp e1 e2 E.AShr E.AShr
{- ORMOLU_ENABLE -}
