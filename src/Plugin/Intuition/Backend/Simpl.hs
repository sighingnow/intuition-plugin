-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition.Backend.Simpl
--
-- A native type constraints solver like Coq's `simpl` tactic.
--
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

module Plugin.Intuition.Backend.Simpl
  ( simpl
  ) where

import Foundation
import Foundation.Collection
import Foundation.Primitive (integralDownsize)

import Control.Monad.Trans.State.Lazy

import Plugin.Intuition.Arg
import Plugin.Intuition.GHC

-- | Try simplify the given Type, using a Bool to indicate if the Type is modified.
simpl :: (?cmdargs :: Arg) => Type -> State Bool Type
simpl ty@(TyVarTy _) = return ty
simpl ty@(AppTy _ _) = return ty
simpl ty@(TyConApp op args) = do
  case (showSDocUnsafe . ppr $ op) of
    "+" -> mapM simpl args >>= \case
      [LitTy (NumTyLit a), LitTy (NumTyLit b)] -> do
        put True
        return $ LitTy (NumTyLit (a + b))
      _ -> return ty
    "*" -> mapM simpl args >>= \case
      [LitTy (NumTyLit a), LitTy (NumTyLit b)] -> do
        put True
        return $ LitTy (NumTyLit (a * b))
      _ -> return ty
    "^" -> mapM simpl args >>= \case
      [LitTy (NumTyLit a), LitTy (NumTyLit b)] -> do
        put True
        return $ LitTy (NumTyLit (a ^ (integralDownsize b :: Word64)))
      _ -> return ty
    "-" -> mapM simpl args >>= \case
      [LitTy (NumTyLit a), LitTy (NumTyLit b)] -> do
        put True
        return $ LitTy (NumTyLit (a - b))
      _ -> return ty

    "Min" -> mapM simpl args >>= \case
      [_, LitTy (NumTyLit a), LitTy (NumTyLit b)] -> do
        put True
        return $ LitTy (NumTyLit (min a b))
      _ -> return ty
    "Max" -> mapM simpl args >>= \case
      [_, LitTy (NumTyLit a), LitTy (NumTyLit b)] -> do
        put True
        return $ LitTy (NumTyLit (max a b))
      _ -> return ty

    _ -> return ty

simpl ty@(ForAllTy _ _) = return ty
simpl ty@(FunTy _ _) = return ty
simpl ty@(LitTy _) = return ty
simpl ty@(CastTy _ _) = return ty
simpl ty@(CoercionTy _) = return ty
