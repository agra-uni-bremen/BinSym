module Expr where

import qualified LibRISCV.Spec.Expr as E
import SymEx.Interpreter
import SymEx.Util
import Test.Tasty
import Test.Tasty.HUnit
import Util
import qualified Z3.Monad as Z3

expressionTests :: TestTree
expressionTests =
  testGroup
    "Expression language tests"
    [ testCase "Equality expression" $ do
        (Just neq) <- Z3.evalZ3 $ do
          x <- mkSymWord32 42
          y <- mkSymWord32 23

          evalE (E.Eq (E.FromImm x) (E.FromImm y)) >>= getInt

        (Just eq) <- Z3.evalZ3 $ do
          x <- mkSymWord32 42
          y <- mkSymWord32 42

          evalE (E.Eq (E.FromImm x) (E.FromImm y)) >>= getInt

        assertEqual "must not be equal" 0 neq
        assertEqual "must be equal" 1 eq,
      testCase "Byte sign extension" $ do
        (Just v) <- Z3.evalZ3 $ do
          x <- mkSymWord32 0xdeadbeef
          evalE (E.SExtByte (E.FromImm x)) >>= getInt

        assertEqual "sign extended lsb" 0xffffffef v
    ]
