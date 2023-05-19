module Memory where

import SymEx.Memory
import SymEx.Util
import Test.Tasty
import Test.Tasty.HUnit
import Util
import qualified Z3.Monad as Z3

memoryTests :: TestTree
memoryTests =
  testGroup
    "Memory tests"
    [ testCase "Write and read byte" $ do
        (Just v) <- Z3.evalZ3 $ do
          mem <- mkMemory 0x0

          addr <- mkSymWord32 0x1000
          value <- mkSymWord8 0xab
          storeByte mem addr value

          loadByte mem addr >>= getInt

        assertEqual "loaded byte must be 0xab" 0xab v,
      testCase "Write and read word" $ do
        (Just v) <- Z3.evalZ3 $ do
          mem <- mkMemory 0x0

          addr <- mkSymWord32 0x0
          value <- mkSymWord32 0xdeadbeef
          storeWord mem addr value

          loadWord mem addr >>= getInt

        assertEqual "" 0xdeadbeef v,
      testCase "Load individual bytes of word" $ do
        (Just v) <- Z3.evalZ3 $ do
          mem <- mkMemory 0x0

          addr <- mkSymWord32 0x0
          value <- mkSymWord32 0xdeadbeef
          storeWord mem addr value

          b0 <- mkSymWord32 0x0 >>= loadByte mem
          b1 <- mkSymWord32 0x1 >>= loadByte mem
          b2 <- mkSymWord32 0x2 >>= loadByte mem
          b3 <- mkSymWord32 0x3 >>= loadByte mem

          getInts [b0, b1, b2, b3]

        assertEqual "" [0xef, 0xbe, 0xad, 0xde] v
    ]
