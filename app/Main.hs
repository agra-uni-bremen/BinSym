{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)
import LibRISCV.CmdLine (BasicArgs (BasicArgs), basicArgs)
import LibRISCV.Effects.Logging.InstructionFetch (runLogInstructionFetchM, runNoLogging)
import LibRISCV.Loader (loadElf, readElf, startAddr)
import LibRISCV.Machine.Interpreter (runInstruction)
import LibRISCV.Spec.AST (buildAST)
import Options.Applicative
import SymEx.Concolic
import SymEx.Interpreter
import SymEx.Memory (storeByteString)
import qualified Z3.Monad as Z3

main'' :: forall z3. (Z3.MonadZ3 z3) => BasicArgs -> z3 ()
main'' (BasicArgs memAddr memSize trace putReg fp) = do
  state@(_, mem) <- liftIO $ mkArchState memAddr memSize

  elf <- liftIO $ readElf fp
  loadElf elf $ storeByteString mem
  entry <- liftIO $ startAddr elf

  let interpreter =
        if trace
          then runReader (evalE @z3, state) . runInstruction symBehavior . runLogInstructionFetchM
          else runReader (evalE @z3, state) . runInstruction symBehavior . runNoLogging

  runM $ interpreter (buildAST @(Concolic Word32) (mkConcrete entry))
  when putReg $
    liftIO $
      dumpState state

main' :: BasicArgs -> IO ()
main' args = Z3.evalZ3 (main'' args)

main :: IO ()
main = main' =<< execParser opts
  where
    opts =
      info
        (basicArgs <**> helper)
        ( fullDesc
            <> progDesc "Symbolic execution of RISC-V machine code"
        )
