-- TODO: Make this interoperable with LibRISCV.Machine.Memory
module SymEx.Memory where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array.IO (IOArray, MArray (newArray), readArray, writeArray)
import Data.Bits (finiteBitSize)
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word16, Word32, Word8)
import LibRISCV (Address)
import qualified LibRISCV.Machine.Memory as M
import SymEx.Concolic
import SymEx.Util
import qualified Z3.Monad as Z3

data Memory = MkMemory
  { startAddr :: Address,
    memArray :: IOArray Address (Concolic Word8)
  }

mkMemory :: (MonadIO m) => Address -> Word32 -> m Memory
mkMemory memStart size = do
  -- TODO: Use an unconstrained value for uninitialized memory
  ary <- liftIO $ newArray (0, size - 1) (mkConcrete 0)
  pure $ MkMemory memStart ary

-- Translate global address to a memory-local address.
toMemAddr :: Memory -> Address -> Address
toMemAddr (MkMemory {startAddr = base}) addr = assert (addr >= base) (addr - base)

------------------------------------------------------------------------

mkBytes' :: (Z3.MonadZ3 z3) => Z3.AST -> z3 [Z3.AST]
mkBytes' bv = do
  bitSize <- bvSize bv
  assert (bitSize `mod` 8 == 0) $
    mapM (\n -> Z3.mkExtract ((n * 8) - 1) ((n - 1) * 8) bv) [1 .. bitSize `div` 8]

mkWord' :: (Z3.MonadZ3 z3) => [Z3.AST] -> z3 Z3.AST
mkWord' = foldM1 (flip Z3.mkConcat)

mkBytes :: (Z3.MonadZ3 z3) => Concolic Word32 -> z3 [Concolic Word8]
mkBytes c = do
  let concrete = M.mkBytes $ getConcrete c
  symbolic <- case getSymbolic c of
    Nothing -> pure $ replicate byteSize Nothing
    Just x -> map Just <$> mkBytes' x

  assert (length concrete == length symbolic) $
    pure (zipWith mkConcolic concrete symbolic)
  where
    byteSize =
      let bitSize = finiteBitSize (getConcrete c)
       in assert (bitSize `mod` 8 == 0) (bitSize `div` 8)

mkWord :: (Z3.MonadZ3 z3) => [Concolic Word8] -> z3 (Concolic Word32)
mkWord bytes = do
  let concrete = M.mkWord $ map getConcrete bytes
  symbolic <-
    if any hasSymbolic bytes
      then Just <$> (mapM getSymbolicDef bytes >>= mkWord')
      else pure Nothing

  pure $ mkConcolic concrete symbolic

------------------------------------------------------------------------

loadByte :: (Z3.MonadZ3 z3) => Memory -> Address -> z3 (Concolic Word8)
loadByte mem = liftIO . readArray (memArray mem) . toMemAddr mem

loadBytes :: (Z3.MonadZ3 z3) => Memory -> Address -> Int -> z3 [Concolic Word8]
loadBytes mem addr numBytes =
  let endAddr = fromIntegral numBytes + addr - 1
   in mapM (loadByte mem) [addr .. endAddr]

loadHalf :: (Z3.MonadZ3 z3) => Memory -> Address -> z3 (Concolic Word16)
loadHalf mem addr = loadBytes mem addr 2 >>= mkWord >>= pure . fmap fromIntegral

loadWord :: (Z3.MonadZ3 z3) => Memory -> Address -> z3 (Concolic Word32)
loadWord mem addr = loadBytes mem addr 4 >>= mkWord

storeByte :: (Z3.MonadZ3 z3) => Memory -> Address -> Concolic Word8 -> z3 ()
storeByte m a = liftIO . writeArray (memArray m) (toMemAddr m a)

storeBytes :: (Z3.MonadZ3 z3) => Memory -> Address -> [Concolic Word8] -> z3 ()
storeBytes m a = mapM_ (\(off, byte) -> storeByte m (a + off) byte) . zip [0 ..]

storeHalf :: (Z3.MonadZ3 z3) => Memory -> Address -> Concolic Word16 -> z3 ()
storeHalf m a w = mkBytes (fmap fromIntegral w) >>= storeBytes m a

storeWord :: (Z3.MonadZ3 z3) => Memory -> Address -> Concolic Word32 -> z3 ()
storeWord m a w = mkBytes w >>= storeBytes m a

storeByteString :: (Z3.MonadZ3 z3) => Memory -> Address -> BSL.ByteString -> z3 ()
storeByteString mem addr bs =
  mapM_ (\(off, val) -> storeByte mem (addr + off) (mkConcrete val)) $
    zip [0 ..] $
      BSL.unpack bs
