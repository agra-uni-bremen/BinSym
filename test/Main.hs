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

  newValue <- mkSymWord32 42
  writeRegister regFile x1 newValue
  readValue <- readRegister regFile x1

  fmap snd $ Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalBv False m) [readValue]

writeZeroRegister :: Z3.Z3 (Maybe [Integer])
writeZeroRegister = do
  regFile <- mkRegFile
  x0 <- mkSymWord32 0

  newValue <- mkSymWord32 42
  writeRegister regFile x0 newValue
  readValue <- readRegister regFile x0

  fmap snd $ Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalBv False m) [readValue]

registerTests :: TestTree
registerTests = testGroup "RegisterFile tests"
  [ testCase "Read default register value" $ do
      (Just v) <- Z3.evalZ3 readZeroReg
      assertEqual "must be zero" [0] v
  , testCase "Write and read register file" $ do
      (Just v) <- Z3.evalZ3 writeRegFile
      assertEqual "must be 42" [42] v
  , testCase "Attempt to write the zero register" $ do
      (Just v) <- Z3.evalZ3 writeZeroRegister
      assertEqual "must be 0" [0] v
  ]
