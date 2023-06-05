module Main where

import ConcolicExpr
import Memory
import SymbolicExpr
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ symbolicTests,
      concolicTests,
      memoryTests
    ]
