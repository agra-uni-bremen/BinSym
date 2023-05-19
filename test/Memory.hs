module Memory where

import Test.Tasty
import Test.Tasty.HUnit

import SymEx.Memory
import SymEx.Util
import Util

import qualified Z3.Monad as Z3

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
