{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Nat where

import GHC.TypeLits
import Data.Proxy

test1 :: Proxy (m + 1) -> Proxy (1 + m)
test1 = id
