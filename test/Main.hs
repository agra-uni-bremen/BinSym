module Main where

import Expr
import Memory
import Register
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ registerTests,
      expressionTests,
      memoryTests
    ]
