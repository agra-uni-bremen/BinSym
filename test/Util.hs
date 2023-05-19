module Util where

import Data.Maybe (catMaybes)
import qualified Z3.Monad as Z3

getInts :: [Z3.AST] -> Z3.Z3 (Maybe [Integer])
getInts values =
  fmap snd $ Z3.withModel $ \m ->
    catMaybes <$> mapM (Z3.evalBv False m) values

getInt :: Z3.AST -> Z3.Z3 (Maybe Integer)
getInt v = do
  ints <- getInts [v]
  pure $ head <$> ints
