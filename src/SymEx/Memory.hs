module SymEx.Memory where

import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import SymEx.Util
import qualified Z3.Monad as Z3

data Memory = Memory
  { start :: Z3.AST,
    array :: IORef Z3.AST
  }

mkMemory :: (Z3.MonadZ3 z3) => Word32 -> z3 Memory
mkMemory addr = do
  -- By default, the memory consists exclusively of
  -- 8-bit bit-vectors as unconstrained symbolic values.
  symbol <- Z3.mkStringSymbol "symbolic-memory"
  defVal <- Z3.mkBvVar symbol 8

  indexSort <- Z3.mkBvSort 32
  byteArray <- Z3.mkConstArray indexSort defVal
  arrayRef <- liftIO $ newIORef byteArray

  startAddr <- mkSymWord32 addr
  pure $ Memory startAddr arrayRef

-- Translate global address to a memory-local address.
--
-- To-Do: Can we perform this on constant arrays only somehow?
toMemAddr :: (Z3.MonadZ3 z3) => Memory -> Z3.AST -> z3 Z3.AST
toMemAddr (Memory startAddr _) addr = Z3.mkBvsub addr startAddr

------------------------------------------------------------------------

loadByte :: (Z3.MonadZ3 z3) => Memory -> Z3.AST -> z3 Z3.AST
loadByte m a = bvSize a >>= \s -> assert (s == 32) loadByte' m a
  where
    loadByte' mem@(Memory _ aryRef) addr = do
      ary <- liftIO $ readIORef aryRef
      toMemAddr mem addr >>= Z3.mkSelect ary

load :: (Z3.MonadZ3 z3) => Memory -> Z3.AST -> Word32 -> z3 Z3.AST
load mem addr numBytes = do
  bytes <-
    mapM (\off -> mkSymWord32 off >>= Z3.mkBvadd addr >>= loadByte mem) $
      assert (numBytes >= 1) [0 .. (numBytes - 1)]
  foldM1 (\acc byte -> Z3.mkConcat byte acc) bytes

storeByte :: (Z3.MonadZ3 z3) => Memory -> Z3.AST -> Z3.AST -> z3 ()
storeByte m a v = do
  addrSize <- bvSize a
  valueSize <- bvSize v
  assert (addrSize == 32 && valueSize == 8) storeByte' m a v
  where
    storeByte' mem@(Memory _ aryRef) addr value = do
      ary <- liftIO $ readIORef aryRef
      new <- toMemAddr mem addr >>= \ra -> Z3.mkStore ary ra value

      liftIO $ writeIORef aryRef new

store :: (Z3.MonadZ3 z3) => Memory -> Z3.AST -> Z3.AST -> z3 ()
store mem addr value = do
  byteSize <- bvSize value >>= \s -> assert (s `mod` 8 == 0) (pure $ s `div` 8)
  bytes <-
    mapM
      (\n -> Z3.mkExtract ((n * 8) - 1) ((n - 1) * 8) value)
      (reverse [1 .. byteSize])

  mapM_ (\(off, byte) -> mkSymWord32 off >>= Z3.mkBvadd addr >>= \a -> storeByte mem a byte) $
    zip [0 ..] bytes
