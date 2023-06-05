module Memory where

import SymEx.Concolic
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
        (c, s) <- Z3.evalZ3 $ do
          mem <- mkMemory 0x0 512

          value <- mkSymbolic 0xab <$> mkSymWord8 0xab
          storeByte mem 0x0 value

          loadByte mem 0x0 >>= concPair

        assertEqual "concrete part" 0xab c
        assertEqual "symbolic part" 0xab s,
      testCase "Write and read word" $ do
        (c, s) <- Z3.evalZ3 $ do
          mem <- mkMemory 0x0 4

          value <- mkSymbolic 0xdeadbeef <$> mkSymWord32 0xdeadbeef
          storeWord mem 0x0 value

          loadWord mem 0x0 >>= concPair

        assertEqual "concrete part" 0xdeadbeef c
        assertEqual "symbolic part" 0xdeadbeef s,
      testCase "Load individual bytes of word" $ do
        lst <- Z3.evalZ3 $ do
          mem <- mkMemory 0x32 64

          value <- mkSymbolic 0xdeadbeef <$> mkSymWord32 0xdeadbeef
          storeWord mem 0x32 value

          concs <- mapM (\off -> loadByte mem (0x32 + off)) [0 .. 3]
          mapM concPair concs

        assertEqual "" [(0xef, 0xef), (0xbe, 0xbe), (0xad, 0xad), (0xde, 0xde)] lst,
      testCase "Single symbolic byte in word" $ do
        (c, s) <- Z3.evalZ3 $ do
          mem <- mkMemory 0x0 4

          let b0 = mkConcrete 0xde
          let b1 = mkConcrete 0xad
          b2 <- mkSymbolic 0xbe <$> mkSymWord8 0xff
          let b3 = mkConcrete 0xef

          storeByte mem 0x0 b3
          storeByte mem 0x1 b2
          storeByte mem 0x2 b1
          storeByte mem 0x3 b0

          loadWord mem 0x0 >>= concPair

        assertEqual "concrete part" 0xdeadbeef c
        assertEqual "symbolic part" 0xdeadffef s
    ]
