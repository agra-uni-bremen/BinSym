module Main where

import Test.Tasty

import Register
import Memory
import Expr

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    registerTests
  , expressionTests
  , memoryTests
  ]
