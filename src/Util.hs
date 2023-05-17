module Util where

import Data.Word (Word32)

import qualified Z3.Monad as Z3

-- Create a symbolic bitvector from a 'Word32'.
mkSymWord32 :: Z3.MonadZ3 z3 => Word32 -> z3 Z3.AST
mkSymWord32 w = Z3.mkBitvector 32 (fromIntegral w)
