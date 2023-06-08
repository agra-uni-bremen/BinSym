{-# LANGUAGE LambdaCase #-}

module SymEx.Tracer
  ( Branch,
    newBranch,
    ExecTrace,
    newExecTrace,
    appendBranch,
    BTree (..),
    ExecTree,
    mkTree,
    addTrace,
    findUnexplored,
  )
where

import Control.Applicative ((<|>))
import SymEx.Cond
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

-- For a given execution trace, return an assignment (represented
-- as a 'Z3.Model') which statisfies all symbolic branch conditions.
-- If such an assignment does not exist, then 'Nothing' is returned.
solveTrace :: (Z3.MonadZ3 z3) => ExecTrace -> z3 (Maybe Z3.Model)
solveTrace trace = do
  assertTrace (init trace)
  let (bool, MkBranch _ ast) = last trace

  isSAT <- makeCond bool ast >>= checkCond
  ret <-
    if isSAT
      then Just <$> Z3.solverGetModel
      else pure Nothing

  -- TODO: Potential optimization: Only pop the constraints from
  -- the solver where the trace diverges from the previous trace.
  Z3.solverReset >> pure ret
  where
    -- Add all conditions enforced by the given 'ExecTrace' to
    -- the solver. Should only be called for n-1 elements of
    -- an 'ExecTrace'. As the last element is not a path condition.
    assertTrace [] = pure ()
    assertTrace t = do
      conds <- mapM (\(b, MkBranch _ c) -> makeCond b c) t
      mapM_ assertCond conds

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
addTrace (Node _ (Just tb) (Just fb)) ((wasTrue, br) : xs)
  | wasTrue = Node br (Just $ addTrace tb xs) (Just fb)
  | otherwise = Node br (Just tb) (Just $ addTrace fb xs)
addTrace (Node _ tb fb) ((wasTrue, br) : xs)
  | canCont tb && wasTrue = Node br (Just $ mkTree xs) fb
  | canCont fb && not wasTrue = Node br tb (Just $ mkTree xs)
  | otherwise = error "addTrace" "trace control flow diverges from existing tree"
-- Leafs are handled above as the leaf is only meaningful if we have the
-- edge leading to it as a context. This case should hence be unrechable.
addTrace Leaf _ = error "addTrace" "unreachable"

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
findUnexplored :: (Z3.MonadZ3 z3) => ExecTree -> z3 (Maybe Z3.Model, ExecTree)
findUnexplored tree =
  case negateBranch tree of
    Nothing -> pure (Nothing, tree)
    Just nt -> do
      let nextTree = addTrace tree nt
      solveTrace nt >>= \case
        Nothing -> findUnexplored nextTree
        Just m -> pure (Just m, nextTree)
