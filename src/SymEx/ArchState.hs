module SymEx.ArchState
  ( ArchState (..),
    mkArchState,
    fromMemory,
    dumpState,
  )
where

import Data.Array.IO (IOArray)
import Data.IORef (IORef, newIORef)
import Data.Word (Word32)
import LibRISCV (Address)
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import Numeric (showHex)
import SymEx.Concolic
import qualified SymEx.Memory as MEM
import SymEx.Tracer (ExecTrace, newExecTrace)

data ArchState = MkArchState
  { getRegs :: REG.RegisterFile IOArray (Concolic Word32),
    getMem :: MEM.Memory,
    getTrace :: IORef ExecTrace
  }

mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
  mem <- MEM.mkMemory memStart memSize
  fromMemory mem

fromMemory :: MEM.Memory -> IO ArchState
fromMemory mem = do
  reg <- REG.mkRegFile $ mkConcrete 0
  ref <- newIORef newExecTrace
  pure $ MkArchState reg mem ref

dumpState :: ArchState -> IO ()
dumpState MkArchState {getRegs = r} = REG.dumpRegs (showHex . getConcrete) r >>= putStr
