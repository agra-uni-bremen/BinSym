{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Word (Word32)
import LibRISCV (Address, RegIdx (A0))
import LibRISCV.CmdLine (BasicArgs (BasicArgs), basicArgs)
import LibRISCV.Effects.Logging.InstructionFetch (runLogInstructionFetchM, runNoLogging)
import LibRISCV.Loader (loadElf, readElf, startAddr)
import LibRISCV.Machine.Interpreter (runInstruction)
import LibRISCV.Machine.Register (writeRegister)
import LibRISCV.Spec.AST (buildAST)
import Options.Applicative
import SymEx.Concolic
import SymEx.Interpreter
import SymEx.Memory (storeByteString)
import SymEx.Tracer
import qualified Z3.Monad as Z3

runPath :: forall z3. (Z3.MonadZ3 z3) => ArchState -> Bool -> Address -> z3 ExecTrace
runPath state verbose entry = do
  let interpreter =
        if verbose
          then runReader (evalE @z3, state) . runInstruction symBehavior . runLogInstructionFetchM
          else runReader (evalE @z3, state) . runInstruction symBehavior . runNoLogging

  runM $ interpreter (buildAST @(Concolic Word32) $ mkConcrete entry)
  liftIO $ readIORef (getTrace state)

main' :: forall z3. (Z3.MonadZ3 z3) => BasicArgs -> z3 ()
main' (BasicArgs memAddr memSize verbose putReg fp) = do
  state@(regs, mem, _) <- liftIO $ mkArchState memAddr memSize

  elf <- liftIO $ readElf fp
  loadElf elf $ storeByteString mem
  entry <- liftIO $ startAddr elf

  -- Make register A0 unconstrained symbolic for testing purposes
  symReg <- mkUncons 0 "A0"
  liftIO $ writeRegister regs A0 symReg

  -- TODO: Run this in a loop
  trace <- runPath state verbose entry

  liftIO (putStrLn $ "Trace: " ++ show trace)
  let negated = negateBranch (mkTree trace)
  case negated of
    Nothing -> liftIO $ putStrLn "no non-negated branch condition"
    Just ne -> do
      liftIO $ (putStrLn $ "negated: " ++ show ne)
      solved <- solveTrace ne
      case solved of
        Nothing -> liftIO $ putStrLn "no model"
        Just m -> do
          liftIO $ putStrLn "alternative path possible:"
          modelStr <- Z3.showModel m
          liftIO $ putStrLn modelStr

  when putReg $
    liftIO $
      dumpState state

main :: IO ()
main = (Z3.evalZ3 . main') =<< execParser opts
  where
    opts =
      info
        (basicArgs <**> helper)
        ( fullDesc
            <> progDesc "Symbolic execution of RISC-V machine code"
        )
