-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition.KnownNat
--
-- Re-export the plugin ghc-typelits-knonwnat and extension the generic KnonwnNat2,
-- so that the caller can have a cleaner dependency.
--
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Plugin.Intuition.KnownNat
  ( -- Re-export
    module GHC.TypeLits.KnownNat
  , module GHC.TypeLits.KnownNat.Solver
    -- Extension
  , module Plugin.Intuition.KnownNat
  ) where

import Foundation
import Foundation.Primitive

import GHC.TypeLits.KnownNat
import GHC.TypeLits.KnownNat.Solver

import Data.Singletons.Prelude.Ord
import Data.Singletons.TypeLits

instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''Max) a b where
  type KnownNatF2 $(nameToSymbol ''Max) = MaxSym0
  natSing2 = let x = natVal (Proxy @a)
                 y = natVal (Proxy @b)
                 z = max x y
              in SNatKn (toNatural (integralDownsize z :: Word64))
  {-# INLINE natSing2 #-}

instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''Min) a b where
  type KnownNatF2 $(nameToSymbol ''Min) = MaxSym0
  natSing2 = let x = natVal (Proxy @a)
                 y = natVal (Proxy @b)
                 z = min x y
              in SNatKn (toNatural (integralDownsize z :: Word64))
  {-# INLINE natSing2 #-}

