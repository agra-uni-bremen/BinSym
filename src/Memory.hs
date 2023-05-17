module Memory where

import Util

import Data.Word (Word32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.IO.Class (liftIO)

import qualified Z3.Monad as Z3

data Memory = Memory { start :: Z3.AST
                     , array :: IORef Z3.AST
                     }

mkMemory :: Z3.MonadZ3 z3 => Word32 -> z3 Memory
mkMemory addr = do
  -- By default, the memory consists exclusively of
  -- 8-bit bit-vectors as unconstrained symbolic values.
  symbol <- Z3.mkStringSymbol "symbolic-memory"
  defVal <- Z3.mkBvVar symbol 8

  indexSort <- Z3.mkBvSort 32
  byteArray <- Z3.mkConstArray indexSort defVal
  arrayRef  <- liftIO $ newIORef byteArray

  startAddr <- mkSymWord32 addr
  pure $ Memory startAddr arrayRef
