module Main where

import ConcolicExpr
import Memory
import SymbolicExpr
import Test.Tasty
import Tracer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ symbolicTests,
      concolicTests,
      memoryTests,
      tracerTests
    ]
