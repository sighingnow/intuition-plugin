-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition.Backend.SMT
--
-- A SMT powered backend for type constraints solving.
--

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Plugin.Intuition.Backend.SMT
  ( MonadZ3
  , Logic(..)
  , Result(..)
  , Constraint
  , Env
  , evalZ3
  , evalZ3With
  , stdOpts
  , smt
  , formula
  , mkAST
  , debugIO
  ) where

import Foundation
import Foundation.Monad (liftIO)
import Foundation.Collection
import Foundation.List.DList

import qualified Prelude
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Z3.Monad

import Plugin.Intuition.Arg
import Plugin.Intuition.GHC

-- | Given and derived constraints (context) provided to SMT solver.
type Constraint = DList AST

-- | Defined App (symbols and their sorts), (used for mkForall).
type Env = DList App

-- | Check if two 'Type' variables are equal in SMT.
smt :: (?cmdargs :: Arg, MonadZ3 m) => Constraint -> Type -> Type -> MaybeT (StateT Env m) EvTerm
smt ctx t1 t2 = do
  f1 <- formula t1
  f2 <- formula t2

  env <- lift $ get

  r <- lift $ lift $ do
    stat <- if null ctx
               then mkEq f1 f2
               else (>>= id) {- join -} $ mkImplies <$> mkAnd (toList ctx) <*> mkEq f1 f2
    prop <- mkForallConst [] (toList env) stat
    when (debug ?cmdargs) $
      debugAST prop
    assert prop
    check

  case r of
    Sat -> lift $ return $
      EvCoercion $
        mkUnivCo (PluginProv "intuition with SMT") Nominal t1 t2
    Unsat -> MaybeT $ return Nothing            -- when unsat, should return a contradiction.
    Undef -> MaybeT $ return Nothing

-- | Translate a Ct to a SMT AST.
mkAST :: (?cmdargs :: Arg, MonadZ3 m) => Ct -> MaybeT (StateT Env m) AST
mkAST ct =
  case ct of
    CNonCanonical (CtWanted pred _ _ _) -> predAST pred
    CFunEqCan (CtGiven pred _ _) _ _ _  -> predAST pred
    CTyEqCan (CtGiven pred _ _) _ _ _   -> predAST pred
    _                                   -> MaybeT $ return Nothing
  where
    predAST pred =
      case classifyPredType pred of
        EqPred NomEq t1 t2 -> do
          t1' <- formula t1
          t2' <- formula t2
          lift $ lift (t1' `mkEq` t2')
        _ -> MaybeT $ return Nothing

-- | Translate a 'Type' variable to a SMT AST.
formula :: (?cmdargs :: Arg, MonadZ3 m) => Type -> MaybeT (StateT Env m) AST
formula (TyVarTy var) = do
  if | isTyVar var -> do
       when (debug ?cmdargs) $
         liftIO . debugIO $ ppr kind

       case (showSDocUnsafe . ppr $ kind) of
         "Nat" -> do

           -- step 1. create Z3 symbol and sort
           (var, app) <- lift $
             lift $ do
               -- NOTICE (TODO) the @var@ can only be made via @sym@ and @sort@, if perform `mkFreshIntVar` and then use `toApp`,
               -- the result proposition would be unsolvable.
               sym <- mkStringSymbol (showSDocUnsafe . ppr $ unique)
               sort <- mkIntSort
               var <- mkVar sym sort
               app <- toApp var
               return (var, app)

           -- step 2. put new symbol into environment.
           lift $ modify (app `cons`)

           -- step 3. return new AST for this variable.
           lift $ return var

         _ -> MaybeT $ return Nothing
     | otherwise -> do
       lift $ lift $ debugNotImplemented "TyVarTy for not Nat"
       MaybeT $ return Nothing
     where
       name = varName var
       unique = varUnique var
       kind = varType var
formula (AppTy f x) = do
  lift $ lift $ debugNotImplemented "AppTy"
  MaybeT $ return Nothing
formula (TyConApp op args) = do
  when (debug ?cmdargs) $
    liftIO . debugIO $ ppr op

  case (showSDocUnsafe . ppr $ op) of
    "+" -> mkArgs args >>= lift . lift . mkAdd
    "*" -> mkArgs args >>= lift . lift . mkMul
    "^" -> MaybeT $ return Nothing        -- currently, (^) is not supported
    "-" -> mkArgs args >>= \case
      [a] -> lift $ lift $ mkUnaryMinus a
      as -> lift $ lift $ mkSub as

    "Nat" -> do
      -- step 1. create Z3 symbol and sort
      (var, app) <- lift $
       lift $ do
         -- NOTICE (TODO) the @var@ can only be made via @sym@ and @sort@, if perform `mkFreshConst` and then use `toApp`,
         -- the result proposition would be unsolvable.
         sym <- mkStringSymbol "Nat"
         sort <- mkUninterpretedSort sym
         var <- mkConst sym sort
         app <- toApp var
         return (var, app)

      -- step 2. put new symbol into environment.
      lift $ modify (app `cons`)

      -- step 3. return new AST for this variable.
      lift $ return var

    "Min" -> mkArgs args >>= \case
      [_, a, b] -> do
        a' <- lift . lift . getNumeralString $ a
        b' <- lift . lift . getNumeralString $ b
        if (null a' || null b') -- a or b is not a number literal
          then MaybeT $ return Nothing
          else lift $ lift $ mkIntNum (min (Prelude.read a' :: Int) (Prelude.read b' :: Int))
      _ -> MaybeT $ return Nothing

    "Max" -> mkArgs args >>= \case
      [_, a, b] -> do
        a' <- lift . lift . getNumeralString $ a
        b' <- lift . lift . getNumeralString $ b
        if (null a' || null b') -- a or b is not a number literal
          then MaybeT $ return Nothing
          else lift $ lift $ mkIntNum (max (Prelude.read a' :: Int) (Prelude.read b' :: Int))
      _ -> MaybeT $ return Nothing

    _ -> do
      lift $ lift $ debugNotImplemented "TyConApp for not +/*/-"
      MaybeT $ return Nothing
    where
      mkArgs as = case as of
        [a] -> sequence $ [formula a]
        [a, b] -> sequence $ [formula a, formula b]
        [a, b, c] -> sequence $ [formula a, formula b, formula c]
        _ -> MaybeT $ return Nothing
formula (ForAllTy _ t) = do
  lift $ lift $ debugNotImplemented "ForAllTy"
  MaybeT $ return Nothing
formula (FunTy a b) = do
  lift $ lift $ debugNotImplemented "FunTy"
  MaybeT $ return Nothing
formula (LitTy lit) =
  case lit of
    NumTyLit i -> lift $ lift $ mkIntNum i             -- not use `mkInteger` as an optimization, for fast.
    StrTyLit s -> MaybeT $ return Nothing
formula (CastTy cond _) = MaybeT $ return Nothing
formula (CoercionTy co) = MaybeT $ return Nothing

debugIO :: SDoc -> IO ()
debugIO = putStrLn . fromList . showSDocUnsafe

debugAST :: MonadZ3 m => AST -> m ()
debugAST ast = do
  s <- astToString ast
  liftIO $ putStrLn . fromString $ s

debugNotImplemented :: MonadZ3 m => String -> m ()
debugNotImplemented = liftIO . putStrLn
