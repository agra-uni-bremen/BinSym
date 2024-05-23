{-# LANGUAGE LambdaCase #-}

module BinSym.Tracer
  ( Branch,
    newBranch,
    ExecTrace,
    newExecTrace,
    appendBranch,
    appendCons,
    BTree (..),
    ExecTree,
    mkTree,
    addTrace,
    findUnexplored,
  )
where

import qualified BinSym.Cond as Cond
import BinSym.Util (prefixLength)
import Control.Applicative ((<|>))
import Data.List (unsnoc)
import qualified Z3.Monad as Z3

-- Represents a branch condition in the executed code
data Branch
  = MkBranch
      Bool -- Whether negation of the branch was attempted
      Z3.AST -- The symbolic branch condition
  deriving (Show, Eq)

-- Create a new branch condition.
newBranch :: Z3.AST -> Branch
newBranch = MkBranch False

-- Create a new branch from an existing branch, thereby updating its metadata.
-- It is assumed that the condition, encoded in the branches, is equal.
fromBranch :: Branch -> Branch -> Branch
fromBranch (MkBranch wasNeg' _) (MkBranch wasNeg ast) = MkBranch (wasNeg || wasNeg') ast

------------------------------------------------------------------------

-- Represents a single execution through a program, tracking for each
-- symbolic branch condition if it was 'True' or 'False'.
type ExecTrace = [(Bool, Branch)]

-- Create a new empty execution tree.
newExecTrace :: ExecTrace
newExecTrace = []

-- Append a branch to the execution trace, denoting via a 'Bool'
-- if the branch was taken or if it was not taken.
appendBranch :: ExecTrace -> Bool -> Branch -> ExecTrace
appendBranch trace wasTrue branch = trace ++ [(wasTrue, branch)]

-- Append a constraint to the execution tree. This constraint must
-- be true and, contrary to appendBranch, negation will not be
-- attempted for it.
appendCons :: ExecTrace -> Z3.AST -> ExecTrace
appendCons trace cons = trace ++ [(True, MkBranch True cons)]

-- For a given execution trace, return an assignment (represented
-- as a 'Z3.Model') which statisfies all symbolic branch conditions.
-- If such an assignment does not exist, then 'Nothing' is returned.
solveTrace :: (Z3.MonadZ3 z3) => ExecTrace -> ExecTrace -> z3 (Maybe Z3.Model)
solveTrace oldTrace newTrace = do
  let newCons = init newTrace
  let oldCons = if null oldTrace then [] else init oldTrace

  let prefix = prefixLength newCons oldCons
  let toDrop = length oldCons - prefix
  Z3.solverPop (toDrop)

  assertTrace (drop prefix newCons)
  let (bool, MkBranch _ ast) = last newTrace

  isSAT <- Cond.new bool ast >>= Cond.check
  if isSAT
    then Just <$> Z3.solverGetModel
    else pure Nothing
  where
    -- Add all conditions enforced by the given 'ExecTrace' to
    -- the solver. Should only be called for n-1 elements of
    -- an 'ExecTrace'. As the last element is not a path condition.
    assertTrace [] = pure ()
    assertTrace t = do
      conds <- mapM (\(b, MkBranch _ c) -> Cond.new b c) t
      mapM_ (\c -> Z3.solverPush >> Cond.assert c) conds

------------------------------------------------------------------------

-- A binary tree.
data BTree a = Node a (Maybe (BTree a)) (Maybe (BTree a)) | Leaf
  deriving (Show, Eq)

-- Execution tree for the exeucted software, represented as follows:
--
--                                 a
--                          True  / \  False
--                               b   …
--                              / \
--                             N   L
--
-- where the edges indicate what happens if branch a is true/false.
-- The left edge covers the true path while the right edge covers the
-- false path.
--
-- The Nothing (N) value indicates that a path has not been explored.
-- In the example above the path `[(True, a), (True, b)]` has not been
-- explored. A Leaf (L) node is used to indicate that a path has been
-- explored but we haven't discored additional branches yet. In the
-- example above the deepest path is hence `[(True a), (False, b)]`.
type ExecTree = BTree Branch

-- Returns 'True' if we can continue exploring on this branch node.
-- This is the case if the node is either a 'Leaf' or 'Nothing'.
canCont :: Maybe (BTree a) -> Bool
canCont Nothing = True
canCont (Just Leaf) = True
canCont _ = False

-- Create a new execution tree from a trace.
mkTree :: ExecTrace -> ExecTree
mkTree [] = Leaf
mkTree [(wasTrue, br)]
  | wasTrue = Node br (Just Leaf) Nothing
  | otherwise = Node br Nothing (Just Leaf)
mkTree ((True, br) : xs) = Node br (Just $ mkTree xs) Nothing
mkTree ((False, br) : xs) = Node br Nothing (Just $ mkTree xs)

-- Add a trace to an existing execution tree. The control flow
-- in the trace must match the existing tree. If it diverges,
-- an error is raised.
--
-- This function prefers the branch nodes from the trace in the
-- resulting 'ExecTree', thus allowing updating their metadata via
-- this function.
--
-- Assertion: The branch encode in the Node and the branch encoded in
-- the trace should also be equal, regarding the encoded condition.
addTrace :: ExecTree -> ExecTrace -> ExecTree
addTrace tree [] = tree
-- The trace takes the True branch and we have taken that previously.
--  ↳ Recursively decent on that branch and look at remaining trace.
addTrace (Node br' (Just tb) fb) ((True, br) : xs) =
  Node (fromBranch br' br) (Just $ addTrace tb xs) fb
-- The trace takes the False branch and we have taken that previously.
--  ↳ Recursively decent on that branch and look at remaining trace.
addTrace (Node br' tb (Just fb)) ((False, br) : xs) =
  Node (fromBranch br' br) tb (Just $ addTrace fb xs)
-- If the trace takes the True/False branch and we have not taken that
-- yet (i.e. canCont is True) we insert the trace at that position.
addTrace (Node br' tb fb) ((wasTrue, br) : xs)
  | canCont tb && wasTrue = Node (fromBranch br' br) (Just $ mkTree xs) fb
  | canCont fb && not wasTrue = Node (fromBranch br' br) tb (Just $ mkTree xs)
  | otherwise = error "unreachable"
-- If we encounter a leaf, this part hasn't been explored yet.
-- That is, we can just insert the trace "as is" at this point.
addTrace Leaf trace = mkTree trace

------------------------------------------------------------------------

-- Negate an unnegated branch in the execution tree and return an
-- 'ExecTrace' which leads to an unexplored execution path. If no
-- such path exists, then 'Nothing' is returned. If such a path
-- exists a concrete variable assignment for it can be calculated
-- using 'solveTrace'.
--
-- The branch node metadata in the resulting 'ExecTree' is updated
-- to reflect that negation of the selected branch node was attempted.
-- If further branches are to be negated, the resulting trace should
-- be added to the 'ExecTree' using 'addTrace' to update the metadata
-- in the tree as well.
negateBranch :: ExecTree -> Maybe ExecTrace
negateBranch Leaf = Nothing
negateBranch (Node (MkBranch wasNeg ast) Nothing _)
  | wasNeg = Nothing
  | otherwise = Just [(True, MkBranch True ast)]
negateBranch (Node (MkBranch wasNeg ast) _ Nothing)
  | wasNeg = Nothing
  | otherwise = Just [(False, MkBranch True ast)]
negateBranch (Node br (Just ifTrue) (Just ifFalse)) =
  do
    -- TODO: Randomly prefer either the left or right child
    (++) [(True, br)] <$> negateBranch ifTrue
    <|> (++) [(False, br)] <$> negateBranch ifFalse

-- Find an assignment (i.e. a 'Z3.Model') that causes exploration
-- of a new execution path through the tested software. This
-- function updates the metadata in the execution tree and thus
-- returns a new execution tree, even if no satisfiable assignment
-- was found.
findUnexplored :: (Z3.MonadZ3 z3) => ExecTree -> ExecTrace -> z3 (Maybe Z3.Model, ExecTrace, ExecTree)
findUnexplored tree lastTrace = do
  case negateBranch tree of
    Nothing -> pure (Nothing, [], tree)
    Just nt -> do
      let nextTree = addTrace tree nt
      solveTrace lastTrace nt >>= \case
        Nothing -> findUnexplored nextTree nt
        Just m -> pure (Just m, nt, nextTree)
