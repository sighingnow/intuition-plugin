-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition
--
-- A GHC plugin used to help solve type equality.
--

{-# LANGUAGE MultiWayIf #-}

module Plugin.Intuition
  ( plugin
  ) where

import Foundation
import Foundation.Collection
import Foundation.Primitive

import Plugin.Intuition.GHC
import Plugin.Intuition.Context
import Plugin.Intuition.Backend.Simpl

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

pluginInit :: TcPluginM Context
pluginInit = Context <$> (return []) -- mapM genOp supportedOps
  where
    genOp (mname, tname, func) = flip (,) func <$> findTyCon mname tname

-- | Conditional execution of 'Applicative' expressions. For example,
--
-- > when debug (putStrLn "Debugging")
--
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
when      :: (Applicative f) => Bool -> f () -> f ()
{-# INLINEABLE when #-}
{-# SPECIALISE when :: Bool -> IO () -> IO () #-}
{-# SPECIALISE when :: Bool -> Maybe () -> Maybe () #-}
when p s  = if p then s else pure ()

-- | The reverse of 'when'.
unless            :: (Applicative f) => Bool -> f () -> f ()
{-# INLINEABLE unless #-}
{-# SPECIALISE unless :: Bool -> IO () -> IO () #-}
{-# SPECIALISE unless :: Bool -> Maybe () -> Maybe () #-}
unless p s        =  if p then pure () else s

pluginSolve ::
     Context
  -> [Ct]    -- ^ Given
  -> [Ct]    -- ^ Derived
  -> [Ct]    -- ^ Wanted
  -> TcPluginM TcPluginResult
pluginSolve ctx given derived wanted = do
  unless (null wanted) $ do
    debug $ text "given:"
    debug $ ppr given
    debug $ text "derived:"
    debug $ ppr derived
    debug $ text "wanted:"
    debug $ ppr wanted
  (solved, unsolved) <- foldlM (\(done, todo) ct -> do
      r <- intuition ctx given derived ct
      case r of
        Nothing -> return (done, ct : todo)
        Just ev -> return ((ev, ct) : done, todo)
    ) ([], []) wanted
  unless (null wanted) $
    tcPluginIO $ do
      putStrLn "Plugin intuition: ----------------------"
      debugIO $ ppr solved
      putStrLn "----------------------------------------"
  return $ if null solved
              then TcPluginOk [] [] -- If the plugin cannot make any progress, it should return TcPluginOk [] []
              else TcPluginOk solved unsolved

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
              t2' = simpl gcdtc t2
          if t1' `eqType` t2'
            then let ev = EvCoercion $ mkUnivCo (PluginProv "intuition") Nominal t1 t2
                  in return (Just ev)
            else return Nothing
        _ -> return Nothing
    _ -> return Nothing

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
