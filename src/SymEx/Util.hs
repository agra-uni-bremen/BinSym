module SymEx.Util
  ( mkSymWord8,
    mkSymWord32,
    bvSize,
    fromBool,
    foldM1,
    copyArray,
  )
where

import Control.Exception (assert)
import Control.Monad (foldM)
import Data.Array.Base (getNumElements, unsafeRead)
import Data.Array.IO (MArray, getBounds, newListArray)
import Data.Ix (Ix)
import Data.Word (Word32, Word8)
import qualified Z3.Monad as Z3

-- Create a symbolic bitvector from a 'Word8'.
mkSymWord8 :: (Z3.MonadZ3 z3) => Word8 -> z3 Z3.AST
mkSymWord8 w = Z3.mkBitvector 8 (fromIntegral w)

-- Create a symbolic bitvector from a 'Word32'.
mkSymWord32 :: (Z3.MonadZ3 z3) => Word32 -> z3 Z3.AST
mkSymWord32 w = Z3.mkBitvector 32 (fromIntegral w)

-- Obtain the size for a bit-vector will crash
-- if the given value is not a Z3 bit-vector.
bvSize :: (Z3.MonadZ3 z3) => Z3.AST -> z3 Int
bvSize ast = Z3.getSort ast >>= Z3.getBvSortSize

-- Convert a predicate to a bitvector representing a truth/false
-- value as defined for branch/compare instructions in RISC-V.
fromBool :: (Z3.MonadZ3 z3) => Z3.AST -> z3 Z3.AST
fromBool boolAst = do
  sort <- Z3.getSort boolAst >>= Z3.getSortKind
  assert (sort == Z3.Z3_BOOL_SORT) (fromBool' boolAst)
  where
    fromBool' boolAst' = do
      trueBV <- mkSymWord32 1
      falseBV <- mkSymWord32 0
      Z3.mkIte boolAst' trueBV falseBV

-- A combination of foldM and foldl1.
foldM1 :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error "empty list"
foldM1 f (x : xs) = foldM f x xs

-- Create a deep copy of a mutable array.
copyArray :: (Ix i, MArray a e m) => a i e -> m (a i e)
copyArray marr = do
  (l, u) <- getBounds marr
  n <- getNumElements marr
  es <- mapM (unsafeRead marr) [0 .. n - 1]
  newListArray (l, u) es
