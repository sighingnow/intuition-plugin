-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition.Context
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Plugin.Intuition.Context
  ( -- * Theorem context and evaluator for  plugins.
    Context(..)
  , supportedOps
  ) where

import Foundation
import Foundation.Collection

import Plugin.Intuition.GHC (TyCon)

-- | Runtime theorem context for the intuition plugin.

newtype Context = Context { runCtx :: [(TyCon, Integer -> Integer -> Integer)] }

instance KeyedCollection Context where
  type Key Context = TyCon
  type Value Context = Integer -> Integer -> Integer
  lookup k c = lookup k (runCtx c)

-- | Supported type-level operators, represented by a list of (moduleName, opName, function) triplets.
supportedOps :: [([Char], [Char], Integer -> Integer -> Integer)]
supportedOps =
  [ ("GHC.TypeLits", "+", (+))
  , ("GHC.TypeLits", "-", (-))
  , ("GHC.TypeLits", "*", (*))
  , ("Plugin.Intuition.TypeLevel", "GCD", gcd)
  ]

-- | GCD.
gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd a b = gcd b (a `mod` b)
