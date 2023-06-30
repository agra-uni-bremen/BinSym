module ConcolicExpr where

import Data.Maybe (fromJust)
import qualified LibRISCV.Effects.Expressions.Expr as E
import SymEx.Concolic
import SymEx.Util (mkSymWord32)
import Test.Tasty
import Test.Tasty.HUnit
import Util
import qualified Z3.Monad as Z3

concolicTests :: TestTree
concolicTests =
  testGroup
    "Concolic expression language tests"
    [ testCase "Add expression" $ do
        (c, Just s) <- Z3.evalZ3 $ do
          x <- mkSymbolic 4 <$> mkSymWord32 4
          y <- mkSymbolic 38 <$> mkSymWord32 38
          r <- evalE $ E.Add (E.FromImm x) (E.FromImm y)

          s <- getInt (fromJust $ getSymbolic r)
          pure (getConcrete r, s)

        assertEqual "Concrete part" 42 c
        assertEqual "Symbolic part" 42 s,
      testCase "Extract expression" $ do
        (c, Just s) <- Z3.evalZ3 $ do
          x <- mkSymbolic 0xdeadbeef <$> mkSymWord32 0xdeadbeef
          r <- evalE (E.Extract 0 16 $ E.FromImm x)

          s <- Z3.simplify (fromJust $ getSymbolic r) >>= getInt
          pure (getConcrete r, s)

        assertEqual "Concrete part" 0xbeef c
        assertEqual "Symbolic part" 0xbeef s
    ]
