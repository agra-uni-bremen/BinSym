{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Word (Word32)
import LibRISCV (RegIdx (A0, SP))
import LibRISCV.CmdLine (BasicArgs (BasicArgs), basicArgs)
import LibRISCV.Effects.Logging.InstructionFetch (runLogInstructionFetchM, runNoLogging)
import LibRISCV.Loader (loadElf, readElf, startAddr)
import LibRISCV.Machine.Interpreter (runInstruction)
import LibRISCV.Machine.Register (writeRegister)
import LibRISCV.Spec.AST (buildAST)
import LibRISCV.Utils (align)
import Options.Applicative
import SymEx.Concolic
import SymEx.Interpreter
import SymEx.Memory (storeByteString)
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

runPath :: forall z3. (Z3.MonadZ3 z3) => BasicArgs -> S.Store -> z3 ExecTrace
runPath (BasicArgs memAddr memSize verbose putReg fp) store = do
  state@(regs, mem, _) <- liftIO $ mkArchState memAddr memSize

  -- Let stack pointer start at end of memory by default.
  -- It must be possible to perform a LW with this address.
  let initalSP = align (memAddr + memSize - 1)
  liftIO $ writeRegister regs SP (mkConcrete initalSP)

  -- TODO: Don't reinitialize memory on every execution
  elf <- liftIO $ readElf fp
  loadElf elf $ storeByteString mem
  entry <- liftIO $ startAddr elf

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

runAll :: forall z3. (Z3.MonadZ3 z3) => Int -> BasicArgs -> S.Store -> Maybe ExecTree -> z3 ()
runAll numPaths args store tree = do
  liftIO $ putStrLn $ "\n##\n# " ++ show numPaths ++ "th concolic execution\n##\n"
  trace <- runPath args store

  -- newTree is the tree including the trace of the last path.
  let newTree = case tree of
        Just t -> addTrace t trace
        Nothing -> mkTree trace

  -- nextTree is the execution tree with updated metadata for
  -- branch nodes which findUnexplored attempted to negate.
  (model, nextTree) <- findUnexplored newTree
  case model of
    Nothing -> pure ()
    Just m -> do
      newStore <- S.fromModel m
      liftIO $ putStrLn ("\nNext assignment:\n" ++ show newStore)
      runAll (numPaths + 1) args newStore (Just nextTree)

main' :: forall z3. (Z3.MonadZ3 z3) => SymbolicArgs -> z3 ()
main' (SymbolicArgs seed args) = do
  -- Optional deterministic random number generation for debugging.
  stdgen <- case seed of
    Just x -> pure $ mkStdGen x
    Nothing -> initStdGen
  liftIO $ setStdGen stdgen

  runAll 1 args S.empty Nothing

main :: IO ()
main = (Z3.evalZ3 . main') =<< execParser opts
  where
    opts =
      info
        (symbolicArgs <**> helper)
        ( fullDesc
            <> progDesc "Symbolic execution of RISC-V machine code"
        )
