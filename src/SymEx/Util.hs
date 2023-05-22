module SymEx.Util (mkSymWord8, mkSymWord32, bvSize, fromBool, foldM1, mayBeTrue, mayBeFalse) where

import Control.Exception (assert)
import Control.Monad (foldM)
import Data.Word (Word32, Word8)
import qualified Z3.Monad as Z3

trueConst :: Word32
trueConst = 1

falseConst :: Word32
falseConst = 0

-- Create a symbolic bitvector from a 'Word8'.
mkSymWord8 :: (Z3.MonadZ3 z3) => Word8 -> z3 Z3.AST
mkSymWord8 w = Z3.mkBitvector 8 (fromIntegral w)

-- Create a symbolic bitvector from a 'Word32'.
mkSymWord32 :: (Z3.MonadZ3 z3) => Word32 -> z3 Z3.AST
mkSymWord32 w = Z3.mkBitvector 32 (fromIntegral w)

-- Obtain the size for a bit-vector will crash
-- if the given value is not a Z3 bit-vector.
bvSize :: (Z3.MonadZ3 z3) => Z3.AST -> z3 Int
bvSize ast = Z3.getSort ast >>= Z3.getBvSortSize

-- Convert a predicate to a bitvector representing a truth/false
-- value as defined for branch/compare instructions in RISC-V.
fromBool :: (Z3.MonadZ3 z3) => Z3.AST -> z3 Z3.AST
fromBool boolAst = do
  sort <- Z3.getSort boolAst >>= Z3.getSortKind
  assert (sort == Z3.Z3_BOOL_SORT) (fromBool' boolAst)
  where
    fromBool' boolAst' = do
      trueBV <- mkSymWord32 trueConst
      falseBV <- mkSymWord32 falseConst
      Z3.mkIte boolAst' trueBV falseBV

-- A combination of foldM and foldl1.
foldM1 :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error "empty list"
foldM1 f (x : xs) = foldM f x xs

-- Convert a Z3 result to a boolean, panic'ing on an unknown result.
fromResult :: Z3.Result -> Bool
fromResult Z3.Sat = True
fromResult Z3.Unsat = False
fromResult Z3.Undef = error "Z3 solver" "unknown result"

-- Check if the given condition is satisfiable.
checkCond :: (Z3.MonadZ3 z3) => Z3.AST -> z3 Bool
checkCond cond = do
  sort <- Z3.getSort cond >>= Z3.getSortKind
  assert (sort == Z3.Z3_BOOL_SORT) (checkCond' cond)
  where
    checkCond' cond' = fromResult <$> Z3.solverCheckAssumptions [cond']

-- Return true if the condition may be satisfiable and false otherwise.
mayBeTrue :: (Z3.MonadZ3 z3) => Z3.AST -> z3 Bool
mayBeTrue cond = do
  bvSize cond >>= \s ->
    assert (s == 32) $
      mkSymWord32 trueConst >>= Z3.mkEq cond >>= checkCond

-- Return true if the condition may be unsatisfiable and false otherwise.
mayBeFalse :: (Z3.MonadZ3 z3) => Z3.AST -> z3 Bool
mayBeFalse cond = do
  bvSize cond >>= \s ->
    assert (s == 32) $
      mkSymWord32 falseConst >>= Z3.mkEq cond >>= checkCond
