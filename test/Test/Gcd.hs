{-# OPTIONS_GHC -fplugin=Plugin.Intuition #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Gcd where

import GHC.TypeLits
import Data.Proxy

import Plugin.Intuition.TypeLevel

test1 :: Proxy (GCD 6 8) -> Proxy 2
test1 = id

