{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SymEx.Interpreter (symBehavior, symEval) where

import Control.Monad.Freer
import Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector as BV
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word (Word32)
import LibRISCV (RegIdx (..))
import LibRISCV.Effects.Expressions.Language
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import LibRISCV.Effects.Operations.Language (Operations (..), Size (..))
import SymEx.ArchState
import SymEx.Concolic
import qualified SymEx.Memory as MEM
import SymEx.Syscall
import SymEx.Tracer
import qualified Z3.Monad as Z3

------------------------------------------------------------------------

-- Convert a concolic value to a register index. This function assumes
-- that a concolic value, used to index the register file, never has a
-- symbolic part.
getRegIdx :: Concolic BV.BV -> RegIdx
getRegIdx = toEnum . fromIntegral . getConcrete

-- Track a new branch in the execution trace.
trackBranch :: (Z3.MonadZ3 z3) => IORef ExecTrace -> Bool -> Z3.AST -> z3 ()
trackBranch ref wasTrue cond = do
  trace <- liftIO $ readIORef ref
  let newTrace = appendBranch trace wasTrue (newBranch cond)
  liftIO $ writeIORef ref newTrace

-- Concretize a concolic value with a potentially symbolic part. That
-- is, if the concolic value has a symbolic part add a constraint to
-- the execution trace which ensures that it matches the concrete part
-- for this execution.
concretize :: (Z3.MonadZ3 z3) => IORef ExecTrace -> Concolic BV.BV -> z3 Word32
concretize ref value = do
  let conc = getConcrete value
  case getSymbolic value of
    Just s -> do
      eq <- Z3.mkBitvector 32 (fromIntegral conc) >>= Z3.mkEq s
      liftIO $ do
        trace <- readIORef ref
        writeIORef ref $ appendCons trace eq
    Nothing -> pure ()
  pure $ fromIntegral conc

-- Implementation of the LibRISCV Operations effect.
symBehavior :: (Z3.MonadZ3 z3) => ArchState -> Operations (Concolic BV.BV) ~> z3
symBehavior state@(MkArchState regFile mem ref _) = \case
  ReadRegister idx -> do
    word <- liftIO $ REG.readRegister regFile (getRegIdx idx)
    pure $ fmap (BV.bitVec 32) word
  WriteRegister idx value -> do
    let word = fmap fromIntegral value
    liftIO $ REG.writeRegister regFile (getRegIdx idx) word
  -- TODO: Refactor the Memory.hs for the new Load consturctor
  Load size a -> do
    addr <- concretize ref a
    case size of
      Byte -> fmap (BV.bitVec 8) <$> MEM.loadByte mem addr
      Half -> fmap (BV.bitVec 16) <$> MEM.loadHalf mem addr
      Word -> fmap (BV.bitVec 32) <$> MEM.loadWord mem addr
  -- TODO: Refactor the Memory.hs for the new Store consturctor
  Store size a v -> do
    addr <- concretize ref a
    case size of
      Byte -> MEM.storeByte mem addr (fmap fromIntegral v)
      Half -> MEM.storeHalf mem addr (fmap fromIntegral v)
      Word -> MEM.storeWord mem addr (fmap fromIntegral v)
  WritePC newPC -> do
    liftIO $ REG.writePC regFile (fromIntegral $ getConcrete newPC)
  ReadPC -> mkConcrete . BV.bitVec 32 <$> liftIO (REG.readPC regFile)
  Exception _ msg -> error "runtime exception" msg
  Ecall _ -> do
    sys <- liftIO $ REG.readRegister regFile A7
    execSyscall state (getConcrete sys)
  Ebreak _ -> liftIO $ putStrLn "EBREAK"

-- Implementation of the LibRISCV expression language effect.
--
-- This implementation assumes that the EvalBool constructor is
-- only used for branch instruction and hence traces all evalCond
-- invocations where the condition has a symbolic value.
symEval :: (Z3.MonadZ3 z3) => IORef ExecTrace -> ExprEval (Concolic BV.BV) ~> z3
symEval ref = \case
  Eval e -> evalE e
  IsTrue e -> do
    conc <- evalE e

    let mayBeTrue = getConcrete conc == 1
    case getSymbolic conc of
      Just br -> trackBranch ref mayBeTrue br
      Nothing -> pure ()

    pure mayBeTrue
  IsFalse e -> do
    conc <- evalE e

    let mayBeFalse = getConcrete conc == 0
    case getSymbolic conc of
      Just br -> trackBranch ref (not mayBeFalse) br
      Nothing -> pure ()

    pure mayBeFalse
