{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SymEx.Interpreter where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import LibRISCV (Address)
import qualified LibRISCV.Decoder.Instruction as I
import qualified LibRISCV.Spec.Expr as E
import LibRISCV.Spec.Operations (Operations (..))
import SymEx.Cond
import qualified SymEx.Memory as MEM
import qualified SymEx.Register as REG
import SymEx.Util
import qualified Z3.Monad as Z3

-- Map a binary operation in the LibRISCV expression language to a Z3 operation.
binOp :: (Z3.MonadZ3 z3) => E.Expr Z3.AST -> E.Expr Z3.AST -> (Z3.AST -> Z3.AST -> z3 Z3.AST) -> z3 Z3.AST
binOp e1 e2 op = do
  bv1 <- evalE e1
  bv2 <- evalE e2
  op bv1 bv2

{- ORMOLU_DISABLE -}
evalE :: Z3.MonadZ3 z3 => E.Expr Z3.AST -> z3 Z3.AST
evalE (E.FromImm e)  = pure e
evalE (E.FromUInt v) = mkSymWord32 v
evalE (E.ZExtByte v) = do
  bv <- evalE v
  bvSize bv >>= \s -> assert (s == 8)  $ Z3.mkZeroExt 24 bv
evalE (E.ZExtHalf v) = do
  bv <- evalE v
  bvSize bv >>= \s -> assert (s == 16) $ Z3.mkZeroExt 16 bv
evalE (E.SExtByte v) = do
  bv <- evalE v
  bvSize bv >>= \s -> assert (s == 8)  $ Z3.mkSignExt 24 bv
evalE (E.SExtHalf v) = do
  bv <- evalE v
  bvSize bv >>= \s -> assert (s == 16) $ Z3.mkSignExt 16 bv
evalE (E.Add e1 e2)  = binOp e1 e2 Z3.mkBvadd
evalE (E.Sub e1 e2)  = binOp e1 e2 Z3.mkBvsub
evalE (E.Eq e1 e2)   = binOp e1 e2 Z3.mkEq     >>= fromBool
evalE (E.Slt e1 e2)  = binOp e1 e2 Z3.mkBvslt  >>= fromBool
evalE (E.Sge e1 e2)  = binOp e1 e2 Z3.mkBvsge  >>= fromBool
evalE (E.Ult e1 e2)  = binOp e1 e2 Z3.mkBvult  >>= fromBool
evalE (E.Uge e1 e2)  = binOp e1 e2 Z3.mkBvuge  >>= fromBool
evalE (E.And e1 e2)  = binOp e1 e2 Z3.mkBvand
evalE (E.Or e1 e2)   = binOp e1 e2 Z3.mkBvor
evalE (E.Xor e1 e2)  = binOp e1 e2 Z3.mkBvxor
evalE (E.LShl e1 e2) = binOp e1 e2 Z3.mkBvshl
evalE (E.LShr e1 e2) = binOp e1 e2 Z3.mkBvlshr
evalE (E.AShr e1 e2) = binOp e1 e2 Z3.mkBvashr
{- ORMOLU_ENABLE -}

------------------------------------------------------------------------

type ArchState = (REG.RegisterFile, MEM.Memory, IORef Word32)

mkArchState :: (Z3.MonadZ3 z3) => Address -> z3 ArchState
mkArchState memStart = do
  reg <- REG.mkRegFile
  mem <- MEM.mkMemory memStart
  instr <- liftIO $ newIORef (0 :: Word32)
  pure (reg, mem, instr)

dumpState :: (Z3.MonadZ3 z3) => ArchState -> z3 ()
dumpState (r, _, _) = do
  REG.dumpRegs r >>= liftIO . putStr

------------------------------------------------------------------------

type SymEnv m = (E.Expr Z3.AST -> m Z3.AST, ArchState)

symBehavior :: (Z3.MonadZ3 z3) => SymEnv z3 -> Operations Z3.AST ~> z3
symBehavior env@(eval, (regFile, mem, instr)) = \case
  DecodeRS1 _ -> I.mkRs1 <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeRS2 _ -> I.mkRs2 <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeRD _ -> I.mkRd <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeImmB _ -> I.immB <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeImmS _ -> I.immS <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeImmU _ -> I.immU <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeImmI _ -> I.immI <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeImmJ _ -> I.immJ <$> (liftIO $ readIORef instr) >>= mkSymWord32
  DecodeShamt _ -> I.mkShamt <$> (liftIO $ readIORef instr) >>= mkSymWord32
  RunIf cond next -> do
    c <- evalE cond >>= makeCond True
    mayBeTrue <- checkCond c
    when mayBeTrue $ do
      assertCond c
      symBehavior env next
  RunUnless cond next -> do
    c <- evalE cond >>= makeCond False
    mayBeFalse <- checkCond c
    when mayBeFalse $ do
      assertCond c
      symBehavior env next
  ReadRegister idx -> REG.readRegister regFile idx
  WriteRegister idx val -> eval val >>= REG.writeRegister regFile idx
  LoadByte addr -> evalE addr >>= MEM.loadByte mem
  LoadHalf addr -> evalE addr >>= MEM.loadHalf mem
  LoadWord addr -> evalE addr >>= MEM.loadWord mem
  LoadInstr addr -> do
    x <- evalE addr >>= MEM.loadWord mem >>= Z3.simplify
    w <- getWord32 x
    liftIO $ writeIORef instr w
    pure (w, x)
  StoreByte addr value -> do
    a <- evalE addr
    v <- evalE value
    MEM.storeByte mem a v
  StoreHalf addr value -> do
    a <- evalE addr
    v <- evalE value
    MEM.storeHalf mem a v
  StoreWord addr value -> do
    a <- evalE addr
    v <- evalE value
    MEM.storeWord mem a v
  WritePC newPC -> eval newPC >>= REG.writePC regFile
  ReadPC -> REG.readPC regFile
  Exception _ msg -> error "runtime exception" msg
  Ecall _ -> liftIO $ putStrLn "ECALL"
  Ebreak _ -> liftIO $ putStrLn "EBREAK"
  Append__ s s' -> symBehavior env s >> symBehavior env s'
