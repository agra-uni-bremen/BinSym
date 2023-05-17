module Register where

import Util

import qualified Z3.Monad as Z3
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- A register file for manipulating general purpose registers and the PC.
data RegisterFile = RegisterFile
                      (IORef Z3.AST) -- Program Counter (32-bit BV)
                      (IORef Z3.AST) -- General purpose registers (Array)

-- Create a new register file, represented internally as a Z3 Array.
mkRegFile :: Z3.MonadZ3 z3 => z3 RegisterFile
mkRegFile = do
  indexSort <- Z3.mkBvSort 32
  zeroValue <- mkSymWord32 0
  regsArray <- Z3.mkConstArray indexSort zeroValue

  pcRef <- liftIO $ newIORef zeroValue
  regRef <- liftIO $ newIORef regsArray

  pure $ RegisterFile pcRef regRef

------------------------------------------------------------------------

-- Read a general purpose register from the register file.
-- Reads of the zero register always return zero.
-- The register index needs to be a given as 32-bit Z3 bit vector.
readRegister :: Z3.MonadZ3 z3 => RegisterFile -> Z3.AST -> z3 Z3.AST
readRegister (RegisterFile _ regRef) index = do
  zero <- mkSymWord32 0
  cond <- Z3.mkEq index zero

  regArray <- liftIO $ readIORef regRef
  regRead <- Z3.mkSelect regArray index
  Z3.mkIte cond zero regRead

-- Return a new register file where the given register was written.
-- The register index needs to be a given as 32-bit Z3 bit vector.
writeRegister :: Z3.MonadZ3 z3 => RegisterFile -> Z3.AST -> Z3.AST -> z3 ()
writeRegister (RegisterFile _ regRef) index value = do
  regArray <- liftIO $ readIORef regRef
  newArray <- Z3.mkStore regArray index value

  liftIO $ writeIORef regRef newArray

-- Read the program counter values as a 32-bit Z3 bit vector.
readPC :: Z3.MonadZ3 z3 => RegisterFile -> z3 Z3.AST
readPC (RegisterFile pc _) = liftIO $ readIORef pc

-- Return a new register file with a modified program counter.
writePC :: Z3.MonadZ3 z3 => RegisterFile -> Z3.AST -> z3 ()
writePC (RegisterFile pc _) newPC = liftIO $ writeIORef pc newPC
