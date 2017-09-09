-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition
--
-- A GHC plugin used to help solve type equality.
--

module Plugin.Intuition
  ( plugin
  ) where

import Foundation
import Foundation.Primitive

import GhcPlugins
import TcPluginM
import TcRnMonad
import Outputable
import Type (PredTree(..), classifyPredType)
import TyCoRep (Type(..), TyLit(..), UnivCoProvenance(..))
import TcEvidence
import Coercion (Coercion(..), mkUnivCo)

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = installTcPlugin
  }

installTcPlugin :: [CommandLineOption] -> Maybe TcPlugin
installTcPlugin _ = Just $ TcPlugin
  { tcPluginInit = pluginInit
  , tcPluginSolve = pluginSolve
  , tcPluginStop = pluginStop
  }

findTyCon :: [Char] -> [Char] -> TcPluginM TyCon
findTyCon mname tname = do
  let m = mkModuleName mname
  found <- findImportedModule m Nothing
  mod <- case found of
    Found _ h -> return h
    _ -> pprPanic "Unable to resolve module looked up in intuition plugin: " (ppr m)
  name <- lookupOrig mod (mkTcOcc tname)
  tcLookupTyCon name

newtype Context = Context { runCtx :: () }

pluginInit :: TcPluginM Context
pluginInit = return $ Context ()

pluginSolve ::
     Context
  -> [Ct]    -- ^ Given
  -> [Ct]    -- ^ Derived
  -> [Ct]    -- ^ Wanted
  -> TcPluginM TcPluginResult
pluginSolve ctx given derived wanted = do
  debug $ ppr given
  debug $ ppr derived
  debug $ ppr wanted
  (solved, unsolved) <- foldlM (\(done, todo) ct -> do
      r <- intuition ctx given derived ct
      case r of
        Nothing -> return (done, ct : todo)
        Just ev -> return ((ev, ct) : done, todo)
    ) ([], []) wanted
  tcPluginIO $ do
    putStrLn "Plugin intuition:"
    debugIO $ ppr solved
  return $ TcPluginOk solved unsolved

-- | Try solve a single constraint.
intuition ::
     Context
  -> [Ct]
  -> [Ct]
  -> Ct
  -> TcPluginM (Maybe EvTerm)
intuition ctx given derived wanted =
  case wanted of
    CNonCanonical (CtWanted pred _ _ _) ->
      case classifyPredType pred of
        EqPred NomEq t1 t2 -> do
          gcdtc <- findTyCon "Plugin.Intuition.TypeLevel" "GCD"
          let t1' = simpl gcdtc t1
          let t2' = simpl gcdtc t2
          if t1' `eqType` t2'
            then let ev = EvCoercion $ mkUnivCo (PluginProv "intuition") Nominal t1 t2
                  in return (Just ev)
            else return Nothing
        _ -> return Nothing
    _ -> return Nothing

-- eqType :: Type -> Type -> Bool
-- eqType (TyVarTy var1) (TyVarTy var2) = var1 == var2
-- eqType (LitTy lit1) (LitTy lit2) = lit1 == lit2
-- eqType _ _ = False

gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd a b = gcd b (a `mod` b)

eval :: TyCon -> Type -> Maybe Integer
eval gcdtc (LitTy (NumTyLit i)) = Just i
eval gcdtc (TyConApp tc [ta, tb])
  | gcdtc == tc = gcd <$> eval gcdtc ta <*> eval gcdtc tb
eval _ _ = Nothing

simpl :: TyCon -> Type -> Type
simpl _ ty@(LitTy (NumTyLit i)) = ty
simpl gcdtc ty@(TyConApp tc [ta, tb]) =
  case eval gcdtc ty of
    Just i -> LitTy (NumTyLit i)
    Nothing -> ty
simpl _ ty = ty

debug :: SDoc -> TcPluginM ()
debug = tcPluginIO . debugIO

debugIO :: SDoc -> IO ()
debugIO = putStrLn . fromList . showSDocUnsafe

pluginStop :: Context -> TcPluginM ()
pluginStop = const (return ())
