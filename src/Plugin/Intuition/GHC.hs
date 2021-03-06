-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition.GHC
--
-- This module provide utility functions to ease the complexity of using
-- GHC APIs. This module also try to erase detailed differences between
-- different version of GHC compiler.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Plugin.Intuition.GHC
  ( -- * Plugin
    Plugin(..)
  , CommandLineOption
  , defaultPlugin
    -- * Modules
  , FindResult(..)
  , findImportedModule
  , lookupOrig
  , tcLookupTyCon
    -- * Constraints and CtEvidence
  , newWanted
    -- * Names
  , OccName
  , mkModuleName
  , mkOccName
  , mkTcOcc
    -- * TcPlugin
  , TcPlugin(..)
  , TcPluginM
  , TcPluginResult(..)
  , tcPluginIO
    -- * Type
  , Ct(..)
  , Role(..)
  , Type(..)
  , TyLit(..)
  , TyCon(..)
  , Var
  , PredTree(..)
  , EqRel(..)
  , varName
  , varUnique
  , varType
  , isTyVar
  , eqType
  , classifyPredType
    -- * Evidence
  , CtEvidence(..)
  , UnivCoProvenance(..)
  , EvTerm(..)
  , Coercion(..)
  , mkReflCo
  , mkTyConAppCo
  , mkUnivCo
    -- * Pretty-print
  , SDoc
  , ppr
  , text
  , pprPanic
  , showSDocUnsafe
    -- * Misc
  , foldlM
  ) where

-- | Plugin
import Plugins (Plugin(..), CommandLineOption, defaultPlugin)
-- | Modules
import TcPluginM (FindResult(..))
import TcPluginM (findImportedModule, tcLookupTyCon, lookupOrig)
-- | Create new constraints (CtEvidence)
import TcPluginM (newWanted)
-- | Names
import Module (mkModuleName)
import OccName (OccName, mkOccName, mkTcOcc)
-- | TcPlugin
import TcRnTypes (TcPlugin(..), TcPluginM, TcPluginResult(..))
import TcPluginM (tcPluginIO)
-- | Type
import TcRnTypes (Ct(..))
import CoAxiom (Role(..))
import TyCoRep (Type(..), TyLit(..))
import TyCon (TyCon(..))
import Var (Var, varName, varUnique, varType, isTyVar)
import Type (PredTree(..), EqRel(..), eqType, classifyPredType)
-- | Evidence
import TcRnTypes (CtEvidence(..))
import TyCoRep (UnivCoProvenance(..))
import TcEvidence (EvTerm(..))
import TyCoRep (Coercion(..))
import Coercion (mkReflCo, mkTyConAppCo, mkUnivCo)
-- | Pretty-print
import Outputable (SDoc, ppr, text, pprPanic, showSDocUnsafe)
-- | Misc
import TcRnMonad (foldlM)
