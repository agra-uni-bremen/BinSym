module SymEx.Syscall (execSyscall) where

import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)
import LibRISCV (RegIdx (A0))
import qualified LibRISCV.Machine.Register as REG
import SymEx.ArchState
import SymEx.Concolic
import System.Exit
import qualified Z3.Monad as Z3

sysExit :: Word32 -> IO ()
sysExit 0 = exitWith ExitSuccess
sysExit c = exitWith (ExitFailure $ fromIntegral c)

execSyscall :: (Z3.MonadZ3 z3) => ArchState -> Word32 -> z3 ()
execSyscall state sysNum = do
  a0 <- getConcrete <$> (liftIO $ REG.readRegister (getRegs state) A0)

  case sysNum of
    93 -> liftIO $ sysExit a0
    _ -> liftIO $ fail "unknown syscall"
