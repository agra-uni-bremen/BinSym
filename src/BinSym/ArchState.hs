module BinSym.ArchState
  ( ArchState (..),
    mkArchState,
    fromMemory,
    dumpState,
  )
where

import BinSym.Concolic
import qualified BinSym.Memory as MEM
import BinSym.Store (Store)
import BinSym.Tracer (ExecTrace, newExecTrace)
import Data.Array.IO (IOArray)
import Data.IORef (IORef, newIORef)
import Data.Word (Word32)
import LibRISCV (Address)
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import Numeric (showHex)

data ArchState = MkArchState
  { getRegs :: REG.RegisterFile IOArray (Concolic Word32),
    getMem :: MEM.Memory,
    getTrace :: IORef ExecTrace,
    getStore :: Store
  }

mkArchState :: Store -> Address -> Word32 -> IO ArchState
mkArchState store memStart memSize = do
  mem <- MEM.mkMemory memStart memSize
  fromMemory store mem

fromMemory :: Store -> MEM.Memory -> IO ArchState
fromMemory store mem = do
  reg <- REG.mkRegFile $ mkConcrete 0
  ref <- newIORef newExecTrace
  pure $ MkArchState reg mem ref store

dumpState :: ArchState -> IO ()
dumpState MkArchState {getRegs = r} = REG.dumpRegs (showHex . getConcrete) r >>= putStr
