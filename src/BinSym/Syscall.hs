module BinSym.Syscall (execSyscall) where

import BinSym.ArchState
import BinSym.Concolic
import qualified BinSym.Memory as MEM
import BinSym.Store (concolicBytes)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)
import LibRISCV (Address, RegIdx (A0, A1))
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import Numeric (showHex)
import System.Exit
import qualified Z3.Monad as Z3

sysExit :: Word32 -> IO ()
sysExit 0 = exitWith ExitSuccess
sysExit c = exitWith (ExitFailure $ fromIntegral c)

makeSymbolic :: (Z3.MonadZ3 z3) => ArchState -> Address -> Int -> z3 ()
makeSymbolic state addr size = do
  let name = "symbolicMemory<" ++ showHex addr ">"
  bytes <- concolicBytes (getStore state) name size

  -- TODO: Implement memory store on (Concolic BV.BV)
  let mem = getMem state
  mapM_
    (\(n, x) -> MEM.storeByte mem (addr + n) (fmap fromIntegral x))
    $ zip [0 ..] bytes

execSyscall :: (Z3.MonadZ3 z3) => ArchState -> Word32 -> z3 ()
execSyscall state sysNum = do
  a0 <- getConcrete <$> liftIO (REG.readRegister (getRegs state) A0)
  a1 <- getConcrete <$> liftIO (REG.readRegister (getRegs state) A1)

  case sysNum of
    93 -> liftIO $ sysExit a0
    96 -> makeSymbolic state a0 (fromIntegral a1)
    _ -> liftIO $ fail "unknown syscall"
