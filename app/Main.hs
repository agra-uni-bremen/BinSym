{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Word (Word32)
import LibRISCV (Address, RegIdx (A0, SP))
import LibRISCV.CmdLine (BasicArgs (BasicArgs, file, memAddr, memStart), basicArgs)
import LibRISCV.Effects.Logging.InstructionFetch (runLogInstructionFetchM, runNoLogging)
import LibRISCV.Loader (loadElf, readElf, startAddr)
import LibRISCV.Machine.Interpreter (runInstruction)
import LibRISCV.Machine.Register (writeRegister)
import LibRISCV.Spec.AST (buildAST)
import LibRISCV.Utils (align)
import Options.Applicative
import SymEx.ArchState
import SymEx.Concolic
import SymEx.Interpreter
import qualified SymEx.Memory as MEM
import qualified SymEx.Store as S
import SymEx.Tracer
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

runPath :: forall z3. (Z3.MonadZ3 z3) => BasicArgs -> EntryState -> S.Store -> z3 ExecTrace
runPath (BasicArgs memBegin memSize verbose putReg _) (mem, entry) store = do
  state <- liftIO $ fromMemory mem
  let regs = getRegs state

  -- Let stack pointer start at end of memory by default.
  -- It must be possible to perform a LW with this address.
  let initalSP = align (memBegin + memSize - 1)
  liftIO $ writeRegister regs SP (mkConcrete initalSP)

  -- Make register A0 unconstrained symbolic for testing purposes
  symReg <- S.getConcolic store "A0"
  liftIO $ writeRegister regs A0 symReg

  let interpreter =
        if verbose
          then runReader (evalE @z3, state) . runInstruction symBehavior . runLogInstructionFetchM
          else runReader (evalE @z3, state) . runInstruction symBehavior . runNoLogging

  runM $ interpreter (buildAST @(Concolic Word32) $ mkConcrete entry)

  ret <- liftIO $ readIORef (getTrace state)
  when putReg $
    liftIO $
      dumpState state
  pure ret

runAll :: forall z3. (Z3.MonadZ3 z3) => Int -> BasicArgs -> EntryState -> S.Store -> Maybe ExecTree -> z3 Int
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

main' :: forall z3. (Z3.MonadZ3 z3) => SymbolicArgs -> z3 ()
main' (SymbolicArgs seed args@(BasicArgs {memAddr = ma, memStart = ms, file = fp})) = do
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
