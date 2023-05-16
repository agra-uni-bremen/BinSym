{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Util

import Control.Monad.Freer
import Data.Word (Word32)
import Data.Array.IO (IOArray)
import LibRISCV (ByteAddrsMem(..), Address)
import LibRISCV.Spec.Operations (Operations(..))
import What4.Concrete (ConcreteVal(..))

import qualified What4.Interface as IF
import qualified Data.BitVector.Sized as BV
import qualified LibRISCV.Spec.Expr as E
import qualified LibRISCV.Machine.Register as REG
import qualified LibRISCV.Machine.Memory as MEM

-- Map a binary operation in the LibRISCV expression language to a What4 operation.
binOp :: (IF.IsExprBuilder sym)
  => sym
  -> E.Expr (IF.SymBV sym 32)
  -> E.Expr (IF.SymBV sym 32)
  -> (sym -> IF.SymBV sym 32 -> IF.SymBV sym 32 -> IO (IF.SymBV sym 32))
  -> IO (IF.SymBV sym 32)
binOp sym e1 e2 op = do
  bv1 <- runExpression sym e1
  bv2 <- runExpression sym e2
  op sym bv1 bv2

runExpression :: IF.IsExprBuilder sym
  => sym
  -> E.Expr (IF.SymBV sym 32)
  -> IO (IF.SymBV sym 32)
runExpression _ (E.FromImm e) = pure e
runExpression sym (E.FromUInt v) = mkSymWord32 sym v
runExpression sym (E.Add e1 e2) = binOp sym e1 e2 (IF.bvAdd :: IF.IsExprBuilder sym => sym -> IF.SymBV sym 32 -> IF.SymBV sym 32 -> IO (IF.SymBV sym 32))
runExpression _ _ = error "expression language operation not implemented"

------------------------------------------------------------------------

type ArchState sym = (REG.RegisterFile IOArray (IF.SymBV sym 32), MEM.Memory IOArray (IF.SymBV sym 32))

-- instance IF.IsExprBuilder sym => ByteAddrsMem (ArchState sym) where
--     storeByteString (_, mem) = MEM.storeByteString mem

mkArchState :: IF.IsExprBuilder sym => sym -> Address -> Word32 -> IO (ArchState sym)
mkArchState sym memStart memSize = do
    reg <- mkSymWord32 sym 0 >>= REG.mkRegFile
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

------------------------------------------------------------------------

type SymEnv sym = (sym, sym -> E.Expr (IF.SymBV sym 32) -> IO (IF.SymBV sym 32), ArchState sym)

symBehavior :: IF.IsExprBuilder sym => SymEnv sym -> Operations (IF.SymBV sym 32) ~> IO
symBehavior (sym, evalE, (regFile, _mem)) = \case
  ReadRegister idx -> do
    bv <- evalE sym $ E.FromImm idx
    case IF.asConcrete bv of
      Just (ConcreteBV _ v) -> REG.readRegister regFile (toEnum (fromIntegral (BV.asUnsigned v)))
      _                     -> error "register index is not concrete"
  _ -> error "operation is not implemented"
