-- TODO: Fix namespace (don't use Cond in names)
module SymEx.Cond (Condition, makeCond, checkCond, negateCond, assertCond, fromResult) where

import Control.Exception (assert)
import Data.Word (Word32)
import SymEx.Util (bvSize, mkSymWord32)
import qualified Z3.Monad as Z3

newtype Condition = MkCond Z3.AST
  deriving (Show, Eq)

trueConst :: Word32
trueConst = 1

falseConst :: Word32
falseConst = 0

-- Create a 32-bit Z3 bit-vector from a bool.
fromBool :: (Z3.MonadZ3 z3) => Bool -> z3 Z3.AST
fromBool True = mkSymWord32 trueConst
fromBool False = mkSymWord32 falseConst

------------------------------------------------------------------------

-- Convert a Z3 result to a boolean, panic'ing on an unknown result.
fromResult :: Z3.Result -> Bool
fromResult Z3.Sat = True
fromResult Z3.Unsat = False
fromResult Z3.Undef = error "Z3 solver" "unknown result"

-- Create a Z3 predicate from a Z3 bit-vector.
makeCond :: (Z3.MonadZ3 z3) => Bool -> Z3.AST -> z3 Condition
makeCond b cond = bvSize cond >>= \s -> assert (s == 32) makeCond'
  where
    makeCond' = MkCond <$> (fromBool b >>= Z3.mkEq cond)

-- Check if the given condition is satisfiable.
checkCond :: (Z3.MonadZ3 z3) => Condition -> z3 Bool
checkCond (MkCond cond) = do
  sort <- Z3.getSort cond >>= Z3.getSortKind
  assert (sort == Z3.Z3_BOOL_SORT) (checkCond' cond)
  where
    checkCond' cond' = fromResult <$> Z3.solverCheckAssumptions [cond']

-- Negate a condition.
negateCond :: (Z3.MonadZ3 z3) => Condition -> z3 Condition
negateCond (MkCond cond) = MkCond <$> Z3.mkNot cond

-- Like Z3.assert but for the 'Condition' type.
assertCond :: (Z3.MonadZ3 z3) => Condition -> z3 ()
assertCond (MkCond c) = Z3.assert c
