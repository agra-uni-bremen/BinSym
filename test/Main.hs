module Main where

import Memory
import Register
import SymbolicExpr
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ registerTests,
      symbolicTests,
      memoryTests
    ]
