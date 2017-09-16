-------------------------------------------------------------------------------
-- |
-- module:          Plugin.Intuition.Arg
--
-- Command line arguments for the intuition plugin.
--

module Plugin.Intuition.Arg
  ( Arg (..)
  ) where

import Foundation

data Arg = Arg { debug :: Bool
               }
