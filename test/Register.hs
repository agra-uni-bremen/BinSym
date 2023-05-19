module Register where

import Test.Tasty
import Test.Tasty.HUnit

import SymEx.Register
import SymEx.Util
import Util

import qualified Z3.Monad as Z3

registerTests :: TestTree
registerTests = testGroup "RegisterFile tests"
  [ testCase "Read default register value" $ do
      (Just v) <- Z3.evalZ3 $ do
        regFile <- mkRegFile
        x1 <- mkSymWord32 1
        readRegister regFile x1 >>= getInt

      assertEqual "default register value is zero" 0 v
  , testCase "Write and read register file" $ do
      (Just v) <- Z3.evalZ3 $ do
        regFile <- mkRegFile
        x1 <- mkSymWord32 1

        newValue <- mkSymWord32 42
        writeRegister regFile x1 newValue
        readRegister regFile x1 >>= getInt

      assertEqual "readRegister must return last written value" 42 v
  , testCase "Attempt to write the zero register" $ do
      (Just v) <- Z3.evalZ3 $ do
        regFile <- mkRegFile
        x0 <- mkSymWord32 0

        newValue <- mkSymWord32 42
        writeRegister regFile x0 newValue
        readRegister regFile x0 >>= getInt

      assertEqual "writes to zero register are ignored" 0 v
  ]
