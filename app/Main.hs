{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (liftIO)
import LibRISCV.CmdLine (BasicArgs (BasicArgs), basicArgs)
import LibRISCV.Effects.Logging.InstructionFetch (runLogInstructionFetchM, runNoLogging)
import LibRISCV.Loader (loadElf, readElf, startAddr)
import LibRISCV.Spec.AST (buildAST)
import Options.Applicative
import SymEx.Interpreter
import SymEx.Memory (storeByteString)
import SymEx.Util (mkSymWord32)
import qualified Z3.Monad as Z3

main'' :: forall z3. (Z3.MonadZ3 z3) => BasicArgs -> z3 ()
main'' (BasicArgs memAddr _memSize trace putReg fp) = do
  state@(_, mem) <- mkArchState memAddr

  elf <- liftIO $ readElf fp
  loadElf elf $ storeByteString mem
  entry <- (liftIO $ startAddr elf) >>= mkSymWord32

  -- TODO: Tracing
  let interpreter =
        if trace
          then runReader (evalE @z3, state) . runInstruction symBehavior . runLogInstructionFetchM
          else runReader (evalE @z3, state) . runInstruction symBehavior . runNoLogging

  runM $ interpreter (buildAST @Z3.AST entry)
  when putReg $
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
