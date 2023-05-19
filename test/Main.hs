import Test.Tasty
import Test.Tasty.HUnit

import SymEx.Memory
import SymEx.Interpreter
import SymEx.Register
import SymEx.Util
import Util

import qualified Z3.Monad as Z3
import qualified LibRISCV.Spec.Expr as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    registerTests
  , expressionTests
  , memoryTests
  ]

------------------------------------------------------------------------

expressionTests :: TestTree
expressionTests = testGroup "Expression language tests"
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
      assertEqual "must be equal" 1 eq
  , testCase "Byte sign extension" $ do
      (Just v) <- Z3.evalZ3 $ do
        x <- mkSymWord32 0xdeadbeef
        evalE (E.SExtByte (E.FromImm x)) >>= getInt

      assertEqual "sign extended lsb" 0xffffffef v
  ]

------------------------------------------------------------------------

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

------------------------------------------------------------------------

memoryTests :: TestTree
memoryTests = testGroup "Memory tests"
  [ testCase "Write and read byte" $ do
      (Just v) <- Z3.evalZ3 $ do
        mem <- mkMemory 0x0

        addr  <- mkSymWord32 0x1000
        value <- mkSymWord8  0xab
        storeByte mem addr value

        loadByte mem addr >>= getInt

      assertEqual "loaded byte must be 0xab" 0xab v
  ]
