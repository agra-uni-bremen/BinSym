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
  x1 <- mkSymWord32 0
  value <- readRegister regFile x1

  fmap snd $ Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalBv False m) [value]

registerTests :: TestTree
registerTests = testGroup "RegisterFile tests"
  [ testCase "Read default register value" $ do
      (Just v) <- Z3.evalZ3 readZeroReg
      assertEqual "must be zero" v [0]
  ]
