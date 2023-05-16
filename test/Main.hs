import Test.Tasty
import Test.Tasty.HUnit

import Register
import Util

import Data.Maybe (catMaybes)
import qualified Z3.Monad as Z3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    registerTests
  ]

------------------------------------------------------------------------

readZeroReg :: Z3.Z3 (Maybe [Integer])
readZeroReg = do
  regFile <- mkRegFile
  x1 <- mkSymWord32 1
  value <- readRegister regFile x1

  fmap snd $ Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalBv False m) [value]

writeRegFile :: Z3.Z3 (Maybe [Integer])
writeRegFile = do
  regFile <- mkRegFile
  x1 <- mkSymWord32 1
  value <- readRegister regFile x1

  newValue <- mkSymWord32 42
  newRegFile <- writeRegister regFile x1 newValue
  readValue <- readRegister newRegFile x1

  fmap snd $ Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalBv False m) [readValue]

registerTests :: TestTree
registerTests = testGroup "RegisterFile tests"
  [ testCase "Read default register value" $ do
      (Just v) <- Z3.evalZ3 readZeroReg
      assertEqual "must be zero" v [0]
  , testCase "Write and read register file" $ do
      (Just v) <- Z3.evalZ3 writeRegFile
      assertEqual "must be 42" v [42]
  ]
