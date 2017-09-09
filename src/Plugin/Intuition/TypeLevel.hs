-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition.TypeLevel
--
-- This module provides many convenient type level operators and constraints
-- that can be handled by the intuition plugin of GHC.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.Intuition.TypeLevel where

import GHC.TypeLits

-- | Type-level GCD on @Nat@.
--
-- The value of GCD of @n@ and @m@ will be computed by the intuition plugin during compile time.
type family GCD (n :: Nat) (m :: Nat) :: Nat where

