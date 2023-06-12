module SymEx.Cond (Condition, new, getAST, check, assert, fromResult) where

import qualified Control.Exception as E
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
new :: (Z3.MonadZ3 z3) => Bool -> Z3.AST -> z3 Condition
new b cond = bvSize cond >>= \s -> E.assert (s == 32) new'
  where
    new' = MkCond <$> (fromBool b >>= Z3.mkEq cond)

-- Extract the Z3 expression from a condition.
getAST :: Condition -> Z3.AST
getAST (MkCond ast) = ast

-- Check if the given condition is satisfiable.
check :: (Z3.MonadZ3 z3) => Condition -> z3 Bool
check (MkCond cond) = do
  sort <- Z3.getSort cond >>= Z3.getSortKind
  E.assert (sort == Z3.Z3_BOOL_SORT) (check' cond)
  where
    check' cond' = fromResult <$> Z3.solverCheckAssumptions [cond']

-- Like Z3.assert but for the 'Condition' type.
assert :: (Z3.MonadZ3 z3) => Condition -> z3 ()
assert (MkCond c) = Z3.assert c
