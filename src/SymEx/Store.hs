module SymEx.Store (Store, empty, fromModel, getConcolic) where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Numeric (readHex)
import SymEx.Concolic
import System.Random (randomIO)
import qualified Z3.Monad as Z3

-- A variable store mapping variable names to concrete values.
data Store = MkStore (Map.Map String Word32)

instance Show Store where
  show (MkStore m) =
    intercalate "\n" $
      map (\(k, v) -> k ++ "\t= " ++ show v) (Map.toList m)

-- Create a new (empty) store.
empty :: Store
empty = MkStore Map.empty

-- Parses the output of 'Z3.modelToString'.
--
-- TODO: This is just a horrible hack, the proper solution to this
-- issues requires figuring out how we can determine the variable
-- names from the model using the Haskell Z3 bindings.
--
-- See also: https://github.com/Z3Prover/z3/blob/1d62964c58991d78bccd9b8aa7d821f5aae77f74/src/model/model_v2_pp.cpp#L78-L81
parseModel :: String -> [(String, Word32)]
parseModel input =
  let pairs = map splitLine (lines input)
   in map (\(n, v) -> (n, fromZ3Hex v)) pairs
  where
    -- Split "A0 -> #x00000001" into the ("A0, "#x00000001").
    splitLine :: String -> (String, String)
    splitLine line =
      let w = words line
       in assert (length w == 3) ((head w), (last w))

    -- Parse an input string like "#x00000001" as 0x00000001.
    fromZ3Hex :: String -> Word32
    fromZ3Hex str = assert (length str == 10) $
      case readHex $ drop 2 str of
        (n, "") : [] -> n
        _ -> error "unexpected modelToString output"

-- Create a variable store from a 'Z3.Model'.
fromModel :: (Z3.MonadZ3 z3) => Z3.Model -> z3 Store
fromModel m = (parseModel <$> Z3.modelToString m) >>= pure . MkStore . Map.fromList

-- Lookup the variable name in the store, if it doesn't exist return a random value.
getOrRand :: (MonadIO m) => Store -> String -> m Word32
getOrRand (MkStore m) name = do
  case Map.lookup name m of
    Just x -> pure x
    Nothing -> liftIO $ randomIO

-- Obtain a unconstrained concolic value from the store.
-- The concrete part is taken from the store or random.
getConcolic :: (Z3.MonadZ3 z3) => Store -> String -> z3 (Concolic Word32)
getConcolic store name = getOrRand store name >>= flip mkUncons name
