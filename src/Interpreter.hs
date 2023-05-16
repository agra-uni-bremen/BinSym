{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Interpreter where

import Util

import Control.Monad.Freer
import Data.Word (Word32)
import Data.Array.IO (IOArray)
import LibRISCV (ByteAddrsMem(..), Address)
import LibRISCV.Spec.Operations (Operations(..))

import qualified Z3.Monad as Z3
import qualified LibRISCV.Spec.Expr as E
import qualified LibRISCV.Machine.Register as REG
import qualified LibRISCV.Machine.Memory as MEM

-- Map a binary operation in the LibRISCV expression language to a Z3 operation.
binOp :: Z3.MonadZ3 z3
  => E.Expr (Z3.AST)
  -> E.Expr (Z3.AST)
  -> (Z3.AST -> Z3.AST -> z3 Z3.AST)
  -> z3 Z3.AST
binOp e1 e2 op = do
  bv1 <- evalE e1
  bv2 <- evalE e2
  op bv1 bv2

evalE :: Z3.MonadZ3 z3 => E.Expr (Z3.AST) -> z3 Z3.AST
evalE (E.FromImm e) = pure e
evalE (E.FromUInt v) = mkSymWord32 v
evalE (E.Add e1 e2) = binOp e1 e2 Z3.mkBvadd
evalE (E.Sub e1 e2) = binOp e1 e2 Z3.mkBvsub
evalE _ = error "expression language operation not implemented"

------------------------------------------------------------------------

type ArchState = (REG.RegisterFile IOArray Z3.AST, MEM.Memory IOArray Z3.AST)

-- instance ByteAddrsMem ArchState where
--   storeByteString (_, mem) = MEM.storeByteString mem

mkArchState :: Address -> Word32 -> Z3.AST -> IO ArchState
mkArchState memStart memSize zeroVal = do
    reg <- REG.mkRegFile zeroVal
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

------------------------------------------------------------------------

type SymEnv m = (E.Expr Z3.AST -> m Z3.AST, ArchState)

symBehavior :: Z3.MonadZ3 z3 => SymEnv z3 -> Operations (Z3.AST) ~> IO
symBehavior _ _ = undefined
