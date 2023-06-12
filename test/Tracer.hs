module Tracer where

import SymEx.Tracer
import SymEx.Util
import Test.Tasty
import Test.Tasty.HUnit
import qualified Z3.Monad as Z3

tracerTests :: TestTree
tracerTests =
  testGroup
    "Tracer tests"
    [ testCase "Create a new tree from a trace" $ do
        (c, t) <- Z3.evalZ3 $ do
          cond <- mkSymWord32 1
          let trace = [(True, newBranch cond)]
          pure (cond, mkTree trace)

        assertEqual "" (Node (newBranch c) (Just Leaf) Nothing) t,
      testCase "Add trace to empty tree" $ do
        (c, t) <- Z3.evalZ3 $ do
          let tree = mkTree []

          cond <- mkSymWord32 1
          let trace = [(True, newBranch cond)]
          pure (cond, addTrace tree trace)

        assertEqual "" (Node (newBranch c) (Just Leaf) Nothing) t,
      testCase "Explore unexplored false branch in root" $ do
        (c, t) <- Z3.evalZ3 $ do
          cond <- mkSymWord32 1
          let trace = [(True, newBranch cond)]

          let tree = mkTree trace
          let newTree = addTrace tree [(False, newBranch cond)]

          pure (cond, newTree)

        assertEqual "" (Node (newBranch c) (Just Leaf) (Just Leaf)) t,
      testCase "Follow path and explore new branch" $ do
        (c1, c2, t) <- Z3.evalZ3 $ do
          cond1 <- mkSymWord32 1
          cond2 <- mkSymWord32 0

          let trace = [(True, newBranch cond1)]
          let tree = mkTree trace

          let newTree = addTrace tree (appendBranch trace True $ newBranch cond2)
          pure (cond1, cond2, newTree)

        assertEqual
          ""
          ( Node
              (newBranch c1)
              ( Just
                  ( Node
                      (newBranch c2)
                      (Just Leaf)
                      Nothing
                  )
              )
              Nothing
          )
          t
    ]
