{-# LANGUAGE FlexibleInstances #-}
-- Ignore orphan instance of FiniteBits for BV.BV.
-- This instance has been proposed upstream <https://github.com/IagoAbal/haskell-bv/pull/9>.
--
-- Unfortunately, we can't disable this warning only for this instance.
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/602
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SymEx.Concolic
  ( Concolic (..),
    mkConcolic,
    mkConcrete,
    hasSymbolic,
    getConcrete,
    getSymbolic,
    getSymbolicDef,
    mkUncons,
    evalE,
  )
where

import qualified Data.BitVector as BV
import Data.Bits (FiniteBits, finiteBitSize)
import Data.Maybe (fromMaybe)
import LibRISCV.Effects.Decoding.Default.Interpreter (Decodable (..))
import qualified LibRISCV.Effects.Expressions.Default.EvalE as I
import qualified LibRISCV.Effects.Expressions.Expr as E
import qualified SymEx.Symbolic as S
import qualified Z3.Monad as Z3

-- Concolic is a tuple of a concrete value (as represented by
-- LibRISCV.Machine.Interpreter) and an optional symbolic value as an
-- SMT bit-vector (as represented by the Haskell Z3 bindings).
--
-- See also: https://en.wikipedia.org/wiki/Concolic_testing
data Concolic a
  = MkConcolic
      a -- Concrete value
      (Maybe Z3.AST) -- Symbolic value (bit-vector)

instance Decodable (Concolic BV.BV) where
  fromWord = mkConcrete . BV.bitVec 32
  toWord = fromIntegral . getConcrete

instance Functor Concolic where
  fmap fn (MkConcolic c s) = MkConcolic (fn c) s

instance FiniteBits BV.BV where
  finiteBitSize = BV.width

-- Create a new concolic value.
mkConcolic :: a -> Maybe Z3.AST -> Concolic a
mkConcolic = MkConcolic

-- Create a concrete concolic value, i.e. a value without a symbolic part.
mkConcrete :: a -> Concolic a
mkConcrete v = MkConcolic v Nothing

-- Create a concolic value with an unconstrained symbolic part.
mkUncons :: (Z3.MonadZ3 z3, FiniteBits a) => a -> String -> z3 (Concolic a)
mkUncons initial name = do
  symbol <- Z3.mkStringSymbol name
  symbolic <- Z3.mkBvVar symbol (finiteBitSize initial)
  pure $ mkConcolic initial (Just symbolic)

-- True if the concolic value has a symbolic part.
hasSymbolic :: Concolic a -> Bool
hasSymbolic (MkConcolic _ Nothing) = False
hasSymbolic (MkConcolic _ (Just _)) = True

-- Extract the concrete part of a concolic value.
getConcrete :: Concolic a -> a
getConcrete (MkConcolic w _) = w

-- Extract the optional symbolic part of a concolic value.
getSymbolic :: Concolic a -> Maybe Z3.AST
getSymbolic (MkConcolic _ s) = s

-- Return a symbolic value for the concolic value, if the concolic value
-- doesn't have a symbolic part then it's concrete part is converted to
-- a Z3 bit-vector instead.
getSymbolicDef :: (Z3.MonadZ3 z3, Integral a, FiniteBits a) => Concolic a -> z3 Z3.AST
getSymbolicDef (MkConcolic c s) = do
  flip fromMaybe s <$> Z3.mkBitvector (finiteBitSize c) (fromIntegral c)

------------------------------------------------------------------------

-- Implementation of the LibRISCV expression language on concolic value.
-- The implementation re-uses the existing implementation of the
-- LibRISCV.Machine.Interpreter for the concrete part of the concolic
-- value. For the symbolic part, the evalE implementation from the
-- SymEx.Symbolic module is used.

-- Perform an unary LibRISCV Expr operation on a concolic value.
unaryOp ::
  (Z3.MonadZ3 z3) =>
  E.Expr (Concolic BV.BV) ->
  (E.Expr BV.BV -> E.Expr BV.BV) ->
  (E.Expr Z3.AST -> E.Expr Z3.AST) ->
  z3 (Concolic BV.BV)
unaryOp e fnConc fnSym = do
  (MkConcolic c s) <- evalE e

  let concrete = I.evalE (fnConc (E.FromImm c))
  symbolic <- case s of
    Just x -> Just <$> S.evalE (fnSym (E.FromImm x))
    Nothing -> pure Nothing

  pure $ MkConcolic concrete symbolic

-- Perform a binary LibRISCV Expr operation on a concolic value.
binaryOp ::
  (Z3.MonadZ3 z3) =>
  E.Expr (Concolic BV.BV) ->
  E.Expr (Concolic BV.BV) ->
  (E.Expr BV.BV -> E.Expr BV.BV -> E.Expr BV.BV) ->
  (E.Expr Z3.AST -> E.Expr Z3.AST -> E.Expr Z3.AST) ->
  z3 (Concolic BV.BV)
binaryOp e1 e2 fnConc fnSym = do
  conc1@(MkConcolic c1 _) <- evalE e1
  conc2@(MkConcolic c2 _) <- evalE e2

  let concrete = I.evalE (fnConc (E.FromImm c1) (E.FromImm c2))
  if hasSymbolic conc1 || hasSymbolic conc2
    then do
      s1 <- getSymbolicDef conc1
      s2 <- getSymbolicDef conc2

      symbolic <- Just <$> S.evalE (fnSym (E.FromImm s1) (E.FromImm s2))
      pure $ MkConcolic concrete symbolic
    else pure $ MkConcolic concrete Nothing

{- ORMOLU_DISABLE -}
-- Evaluate a LibRISCV expression on a 'Concolic' value.
evalE :: Z3.MonadZ3 z3 => E.Expr (Concolic BV.BV) -> z3 (Concolic BV.BV)
evalE (E.FromImm e)     = pure e
evalE (E.FromInt n v)   = pure $ mkConcrete (BV.bitVec n v)
evalE (E.ZExt n e)      = unaryOp e (E.ZExt n) (E.ZExt n)
evalE (E.SExt n e)      = unaryOp e (E.SExt n) (E.SExt n)
evalE (E.Extract i l e) = unaryOp e (E.Extract i l) (E.Extract i l)
evalE (E.Add e1 e2)     = binaryOp e1 e2 E.Add E.Add
evalE (E.Sub e1 e2)     = binaryOp e1 e2 E.Sub E.Sub
evalE (E.Eq e1 e2)      = binaryOp e1 e2 E.Eq E.Eq
evalE (E.Slt e1 e2)     = binaryOp e1 e2 E.Slt E.Slt
evalE (E.Sge e1 e2)     = binaryOp e1 e2 E.Sge E.Sge
evalE (E.Ult e1 e2)     = binaryOp e1 e2 E.Ult E.Ult
evalE (E.Uge e1 e2)     = binaryOp e1 e2 E.Uge E.Uge
evalE (E.And e1 e2)     = binaryOp e1 e2 E.And E.And
evalE (E.Or e1 e2)      = binaryOp e1 e2 E.Or E.Or
evalE (E.Xor e1 e2)     = binaryOp e1 e2 E.Xor E.Xor
evalE (E.LShl e1 e2)    = binaryOp e1 e2 E.LShl E.LShl
evalE (E.LShr e1 e2)    = binaryOp e1 e2 E.LShr E.LShr
evalE (E.AShr e1 e2)    = binaryOp e1 e2 E.AShr E.AShr
evalE (E.Mul e1 e2)     = binaryOp e1 e2 E.Mul E.Mul
evalE (E.SDiv e1 e2)    = binaryOp e1 e2 E.SDiv E.SDiv
evalE (E.UDiv e1 e2)    = binaryOp e1 e2 E.UDiv E.UDiv
evalE (E.SRem e1 e2)    = binaryOp e1 e2 E.SRem E.SRem
evalE (E.URem e1 e2)    = binaryOp e1 e2 E.URem E.URem
{- ORMOLU_ENABLE -}
