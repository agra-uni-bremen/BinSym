{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import BinSym.ArchState
import BinSym.Concolic
import BinSym.Interpreter
import qualified BinSym.Memory as MEM
import qualified BinSym.Store as S
import BinSym.Tracer
import Control.Monad (when)
import Control.Monad.Freer (interpretM, runM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector as BV
import Data.IORef (newIORef, readIORef)
import Data.Word (Word32)
import LibRISCV (Address, RegIdx (SP), align)
import LibRISCV.CmdLine (BasicArgs (BasicArgs, file, memAddr, memSize), basicArgs)
import LibRISCV.Effects.Decoding.Default.Interpreter (defaultDecoding)
import LibRISCV.Effects.Logging.Default.Interpreter (defaultLogging, noLogging)
import LibRISCV.Effects.Operations.Default.Machine.Register (writeRegister)
import LibRISCV.Loader (loadElf, readElf, startAddr)
import LibRISCV.Semantics (buildAST)
import Options.Applicative
import System.Random (initStdGen, mkStdGen, setStdGen)
import qualified Z3.Monad as Z3

data SymbolicArgs = SymbolicArgs
  { randSeed :: Maybe Int,
    base :: BasicArgs
  }

{- ORMOLU_DISABLE -}
symbolicArgs :: Parser SymbolicArgs
symbolicArgs =
  SymbolicArgs
    <$> optional
      (option auto
        ( long "random-seed"
        <> help "Seed for the random number generator"))
    <*> basicArgs
{- ORMOLU_ENABLE -}

------------------------------------------------------------------------

-- TODO: Add proper data type to track all symbolic execution state.
type EntryState = (MEM.Memory, Address)

runPath :: BasicArgs -> EntryState -> S.Store -> Z3.Z3 ExecTrace
runPath (BasicArgs memBegin size verbose putReg _) (mem, entry) store = do
  state <- liftIO $ fromMemory store mem
  let regs = getRegs state

  -- Let stack pointer start at end of memory by default.
  -- It must be possible to perform a LW with this address.
  let initalSP = align (memBegin + size - 1)

  instRef <- liftIO $ newIORef (0 :: Word32)
  let interpreter =
        interpretM (symBehavior state)
          . interpretM (symEval $ getTrace state)
          . interpretM (defaultDecoding @(Concolic BV.BV) instRef)
          . interpretM (if verbose then defaultLogging else noLogging)
  runM $ interpreter $ do
    liftIO $ writeRegister regs SP (mkConcrete initalSP)
    buildAST @32 (mkConcrete $ BV.bitVec 32 entry)

  ret <- liftIO $ readIORef (getTrace state)
  when putReg $
    liftIO $
      dumpState state
  pure ret

runAll :: Int -> BasicArgs -> EntryState -> S.Store -> Maybe ExecTree -> Z3.Z3 Int
runAll numPaths args es store tree = do
  liftIO $ putStrLn $ "\n##\n# " ++ show numPaths ++ "th concolic execution\n##\n"
  trace <- runPath args es store

  -- newTree is the tree including the trace of the last path.
  let newTree = case tree of
        Just t -> addTrace t trace
        Nothing -> mkTree trace

  -- nextTree is the execution tree with updated metadata for
  -- branch nodes which findUnexplored attempted to negate.
  (model, nextTree) <- findUnexplored newTree
  case model of
    Nothing -> pure numPaths
    Just m -> do
      newStore <- S.fromModel m
      liftIO $ putStrLn ("\nNext assignment:\n" ++ show newStore)
      runAll (numPaths + 1) args es newStore (Just nextTree)

------------------------------------------------------------------------

main' :: SymbolicArgs -> Z3.Z3 ()
main' (SymbolicArgs seed args@(BasicArgs {memAddr = ma, memSize = ms, file = fp})) = do
  -- Initial memory state, copied for each execution
  mem <- MEM.mkMemory ma ms
  elf <- liftIO $ readElf fp
  loadElf elf $ MEM.storeByteString mem
  entry <- liftIO $ startAddr elf

  -- Optional deterministic random number generation for debugging.
  stdgen <- case seed of
    Just x -> pure $ mkStdGen x
    Nothing -> initStdGen
  liftIO $ setStdGen stdgen

  numPaths <- runAll 1 args (mem, entry) S.empty Nothing
  liftIO $ putStrLn ("\n---\nUnique paths found: " ++ show numPaths)

main :: IO ()
main = (Z3.evalZ3 . main') =<< execParser opts
  where
    opts =
      info
        (symbolicArgs <**> helper)
        ( fullDesc
            <> progDesc "Symbolic execution of RISC-V machine code"
        )
