module Octogulf.Types (
   HashTable(..),
   OctogulfState(..),
   Octomonad(..),
   Value(..)
  ) where

import qualified Data.HashTable.IO as H
import Control.Monad.State

type HashTable k v = H.CuckooHashTable k v

type OctogulfState = (HashTable String Value, [HashTable String Value])
type Octomonad a = StateT OctogulfState IO a

data Value = ValueInteger Integer | ValueString String | ValueNULL
  deriving (Show, Eq, Read, Ord)
