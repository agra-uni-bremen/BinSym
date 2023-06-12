module Util where

import Data.Bits (FiniteBits)
import Data.Maybe (catMaybes, fromJust)
import SymEx.Concolic
import qualified Z3.Monad as Z3

mkSymbolic :: a -> Z3.AST -> Concolic a
mkSymbolic c s = MkConcolic c (Just s)

concPair :: (Z3.MonadZ3 z3, FiniteBits a, Integral a) => Concolic a -> z3 (a, Integer)
concPair conc = do
  s <- getInt (fromJust $ getSymbolic conc)
  let c = getConcrete conc
  pure (c, fromJust s)

getInts :: (Z3.MonadZ3 z3) => [Z3.AST] -> z3 (Maybe [Integer])
getInts values =
  fmap snd $ Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalBv False m) values

getInt :: (Z3.MonadZ3 z3) => Z3.AST -> z3 (Maybe Integer)
getInt v = do
  ints <- getInts [v]
  pure $ head <$> ints
