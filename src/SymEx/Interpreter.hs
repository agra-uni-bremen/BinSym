{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SymEx.Interpreter (symBehavior) where

import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word (Word32)
import LibRISCV (RegIdx (..))
import qualified LibRISCV.Decoder.Instruction as I
import qualified LibRISCV.Machine.Register as REG
import qualified LibRISCV.Spec.Expr as E
import LibRISCV.Spec.Operations (Operations (..))
import SymEx.ArchState
import SymEx.Concolic
import qualified SymEx.Memory as MEM
import SymEx.Syscall
import SymEx.Tracer
import qualified Z3.Monad as Z3

------------------------------------------------------------------------

type SymEnv m = (E.Expr (Concolic Word32) -> m (Concolic Word32), ArchState)

getRegIdx :: Concolic Word32 -> RegIdx
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
concretize :: (Z3.MonadZ3 z3) => IORef ExecTrace -> Concolic Word32 -> z3 Word32
concretize ref value = do
  let conc = getConcrete value
  case getSymbolic value of
    Just s -> do
      eq <- Z3.mkBitvector 32 (fromIntegral conc) >>= Z3.mkEq s
      liftIO $ do
        trace <- readIORef ref
        writeIORef ref $ appendCons trace eq
    Nothing -> pure ()
  pure conc

symBehavior :: (Z3.MonadZ3 z3) => SymEnv z3 -> Operations (Concolic Word32) ~> z3
symBehavior env@(eval, state@(MkArchState regFile mem ref)) = \case
  DecodeRS1 instr -> pure . mkConcrete . I.mkRs1 $ getConcrete instr
  DecodeRS2 instr -> pure . mkConcrete . I.mkRs2 $ getConcrete instr
  DecodeRD instr -> pure . mkConcrete . I.mkRd $ getConcrete instr
  DecodeImmB instr -> pure . mkConcrete . I.immB $ getConcrete instr
  DecodeImmS instr -> pure . mkConcrete . I.immS $ getConcrete instr
  DecodeImmU instr -> pure . mkConcrete . I.immU $ getConcrete instr
  DecodeImmI instr -> pure . mkConcrete . I.immI $ getConcrete instr
  DecodeImmJ instr -> pure . mkConcrete . I.immJ $ getConcrete instr
  DecodeShamt instr -> pure . mkConcrete . I.mkShamt $ getConcrete instr
  RunIf cond next -> do
    conc <- evalE cond

    let mayBeTrue = getConcrete conc == 1
    when mayBeTrue $ do
      symBehavior env next

    case getSymbolic conc of
      Just br -> trackBranch ref mayBeTrue br
      Nothing -> pure ()
  RunUnless cond next -> do
    conc <- evalE cond

    let mayBeFalse = getConcrete conc == 0
    when mayBeFalse $ do
      symBehavior env next

    case getSymbolic conc of
      -- Track branch check for ==1 if True is supplied here.
      -- Hence, we need to negate mayBeFalse here as we want ==0.
      Just br -> trackBranch ref (not mayBeFalse) br
      Nothing -> pure ()
  ReadRegister idx -> do
    conc <- evalE $ E.FromImm idx
    liftIO $ REG.readRegister regFile (getRegIdx conc)
  WriteRegister idx val -> do
    conc <- evalE $ E.FromImm idx
    value <- evalE val
    liftIO $ REG.writeRegister regFile (getRegIdx conc) value
  LoadByte a -> do
    addr <- evalE a >>= concretize ref
    byte <- MEM.loadByte mem addr
    pure $ fmap fromIntegral byte
  LoadHalf a -> do
    addr <- evalE a >>= concretize ref
    half <- MEM.loadHalf mem addr
    pure $ fmap fromIntegral half
  LoadWord a -> do
    addr <- evalE a >>= concretize ref
    MEM.loadWord mem addr
  LoadInstr a -> do
    addr <- evalE a >>= concretize ref
    inst <- MEM.loadWord mem addr
    pure (getConcrete inst, inst)
  StoreByte a v -> do
    addr <- evalE a >>= concretize ref
    value <- evalE v
    MEM.storeByte mem addr (fmap fromIntegral value)
  StoreHalf a v -> do
    addr <- evalE a >>= concretize ref
    value <- evalE v
    MEM.storeHalf mem addr (fmap fromIntegral value)
  StoreWord a v -> do
    addr <- evalE a >>= concretize ref
    value <- evalE v
    MEM.storeWord mem addr value
  WritePC newPC -> do
    conc <- eval newPC
    liftIO $ REG.writePC regFile (getConcrete conc)
  ReadPC -> mkConcrete <$> liftIO (REG.readPC regFile)
  Exception _ msg -> error "runtime exception" msg
  Ecall _ -> do
    sys <- liftIO $ REG.readRegister regFile A7
    execSyscall state (getConcrete sys)
  Ebreak _ -> liftIO $ putStrLn "EBREAK"
  Append__ s s' -> symBehavior env s >> symBehavior env s'
