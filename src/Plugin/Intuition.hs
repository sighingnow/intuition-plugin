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

import Control.Monad (when)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy

import Plugin.Intuition.Arg
import Plugin.Intuition.GHC
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

  (solved, unsolved) <- tcPluginIO $
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
          iter (done, todo) ct = do
            r <- runMaybeT $
              intuition ctx given derived ct
            case r of
              Nothing -> return (done, ct : todo)
              Just ev -> return ((ev, ct) : done, todo)
        foldlM iter ([], []) wanted

  when (debug && (not . null) wanted) $
    tcPluginIO $ do
      putStrLn "Plugin intuition: ----------------------"
      debugIO $ ppr solved
      putStrLn "----------------------------------------"

  return $ if null solved
              then TcPluginOk [] []     -- If the plugin cannot make any progress, it should return TcPluginOk [] []
              else TcPluginOk solved [] -- the second arg is new works, not unsolved old works,
                                        -- see https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker#Callingpluginsfromthetypechecker

-- | Try solve a single constraint.
intuition ::
     (?cmdargs :: Arg, MonadZ3 m)
  => Constraint
  -> [Ct]
  -> [Ct]
  -> Ct
  -> MaybeT (StateT Env m) EvTerm
intuition ctx given derived wanted =
  case wanted of
    CNonCanonical (CtWanted pred _ _ _) ->
      case classifyPredType pred of
        EqPred NomEq t1 t2 -> smt ctx t1 t2
        _ -> MaybeT $ return Nothing
    _ -> MaybeT $ return Nothing
