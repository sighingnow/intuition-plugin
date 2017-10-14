-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition
--
-- A GHC plugin used to help solve type equality.
--

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Plugin.Intuition
  ( plugin
  ) where

import Foundation
import Foundation.Collection
import Foundation.Monad (liftIO)

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy

import Plugin.Intuition.Arg
import Plugin.Intuition.GHC
import Plugin.Intuition.Backend.Simpl
import Plugin.Intuition.Backend.SMT

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = installTcPlugin
  }

installTcPlugin :: [CommandLineOption] -> Maybe TcPlugin
installTcPlugin cmdargs = Just $ TcPlugin
  { tcPluginInit = pluginInit cmdargs
  , tcPluginSolve = pluginSolve
  , tcPluginStop = const (return ())
  }

-- | Progress given by the intuition plugin.
data Progress = DictCan EvTerm Ct     -- ^ The CDictCan is simplified.
              | NonCanonical EvTerm   -- ^ The CNonCanonical is given a proof (EvTerm)

pluginInit :: [CommandLineOption] -> TcPluginM Arg
pluginInit cmdargs = do
  let debug = "--debug" `elem` cmdargs
  return $ Arg debug

pluginSolve ::
     Arg
  -> [Ct]    -- ^ Given
  -> [Ct]    -- ^ Derived
  -> [Ct]    -- ^ Wanted
  -> TcPluginM TcPluginResult
pluginSolve cmdargs@Arg{..} given derived wanted = do

  when (debug && (not . null) wanted) $ tcPluginIO $ do
    debugIO $ text "given:"
    debugIO $ ppr given
    debugIO $ text "derived:"
    debugIO $ ppr derived
    debugIO $ text "wanted:"
    debugIO $ ppr wanted

  let ?cmdargs = cmdargs

  -- Solver based on Z3 (needs IO)
  (solved1, newly1) <- tcPluginIO $
    evalZ3With (Just QF_LIA) stdOpts $ do
    -- evalZ3With Nothing stdOpts $ do
      -- prepare given and derived
      (ctx, env) <- flip runStateT (mempty :: Env) $ do
        let
          iter assumptions ct = do
            r <- runMaybeT $
              mkAST ct
            case r of
              Nothing -> return assumptions
              Just assume -> return (assume `cons` assumptions)
        foldlM iter (mempty :: Constraint) given

      -- perform SMT solver.
      flip evalStateT env $ do
        let
          iter (done, newly) ct = do
            r <- runMaybeT $
              intuitionZ3 ctx given derived ct
            case r of
              Just (NonCanonical ev) -> return ((ev, ct) : done, newly)
              Nothing -> return (done, newly)
        foldlM iter ([], []) wanted

  -- Solver based on pure GHC API (needs TcPluginM), perform custom Simpl solver
  (solved2, newly2) <- do
      let
        iter (done, newly) ct = do
          r <- runMaybeT $
            intuitionSimpl given derived ct
          case r of
            Just (DictCan ev ct') -> return ((ev, ct) : done, ct' : newly)
            Nothing -> return (done, newly)
      foldlM iter ([], []) wanted

  let solved = solved1 <> solved2
      newly = newly1 <> newly2

  when (debug && (not . null) wanted) $
    tcPluginIO $ do
      putStrLn "Plugin intuition: ----------------------"
      debugIO $ ppr solved
      putStrLn "----------------------------------------"
      debugIO $ ppr newly
      putStrLn "----------------------------------------"

  -- return $ if null solved
  --             then TcPluginOk [] newly     -- If the plugin cannot make any progress, it should return TcPluginOk [] []
  --             else TcPluginOk solved newly -- the second arg is new works, not unsolved old works,
  --                                          -- see https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker#Callingpluginsfromthetypechecker
  return $ TcPluginOk solved newly

-- | Try solve a single constraint.
intuitionSimpl ::
     (?cmdargs :: Arg)
  => [Ct]
  -> [Ct]
  -> Ct
  -> MaybeT TcPluginM Progress
intuitionSimpl given derived wanted =
  case wanted of
    CDictCan (CtWanted pred _ _ loc) cls ts pendsc ->
      case classifyPredType pred of
        ClassPred pcls pts -> do
          let iter ty (rty, simplified) = let (ty', simplified') = runState (simpl ty) simplified
                                           in (ty' : rty, simplified || simplified')
          let (rty, simplified) = foldr iter ([], False) pts
          if simplified
            then MaybeT $ do
              let con' = case pred of
                            TyConApp con kts -> con
                            _ -> error "intuition.CDictCan: impossible happens!"
                  pred' = TyConApp con' rty
                  ts' = rty
              ev' <- newWanted loc pred'
              let ct' = CDictCan ev' cls ts' pendsc
                  evterm = EvCoercion $ mkTyConAppCo Nominal
                                                     con'
                                                     (fmap (\(t, t') ->
                                                       mkUnivCo (PluginProv "intuition with SMT") Nominal t t') (zip ts ts'))
              return $ Just $ DictCan evterm ct'
            else MaybeT $ return Nothing
        _ -> MaybeT $ return Nothing
    _ -> MaybeT $ return Nothing

-- | Try put a single constraint into Z3's context.
intuitionZ3 ::
     (?cmdargs :: Arg, MonadZ3 m)
  => Constraint
  -> [Ct]
  -> [Ct]
  -> Ct
  -> MaybeT (StateT Env m) Progress
intuitionZ3 ctx given derived wanted =
  case wanted of
    CNonCanonical (CtWanted pred _ _ _) ->
      case classifyPredType pred of
        EqPred NomEq t1 t2 -> NonCanonical <$> smt ctx t1 t2
        _ -> MaybeT $ return Nothing
    _ -> MaybeT $ return Nothing
