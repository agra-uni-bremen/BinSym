{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Interpreter where

import Util
import qualified Register as REG

import Control.Monad.Freer
import Data.Word (Word32)
import Data.Array.IO (IOArray)
import LibRISCV (ByteAddrsMem(..), Address)
import LibRISCV.Spec.Operations (Operations(..))
import Control.Monad.IO.Class (liftIO)

import qualified Z3.Monad as Z3
import qualified LibRISCV.Spec.Expr as E
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

type ArchState = (REG.RegisterFile, MEM.Memory IOArray Z3.AST)

-- instance ByteAddrsMem ArchState where
--   storeByteString (_, mem) = MEM.storeByteString mem

mkArchState :: Z3.MonadZ3 z3 => Address -> Word32 -> z3 ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile
    mem <- liftIO $ MEM.mkMemory memStart memSize
    pure (reg, mem)

------------------------------------------------------------------------

type SymEnv m = (E.Expr Z3.AST -> m Z3.AST, ArchState)

symBehavior :: Z3.MonadZ3 z3 => SymEnv z3 -> Operations (Z3.AST) ~> z3
symBehavior (eval, (regFile, _mem)) = \case
  ReadRegister idx -> REG.readRegister regFile idx
  WriteRegister idx val -> eval val >>= REG.writeRegister regFile idx
  WritePC newPC -> eval newPC >>= REG.writePC regFile
  ReadPC -> REG.readPC regFile
