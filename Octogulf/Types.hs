module Octogulf.Types (
   HashTable(..),
   OctogulfState(..),
   Octomonad(..),
   Value(..),
   Procedure(..),
   Statement(..)
  ) where

import qualified Data.HashTable.IO as H
import Control.Monad.State

type HashTable k v = H.CuckooHashTable k v

type OctogulfState = (HashTable String Value, [HashTable String Value], HashTable String Procedure)
type Octomonad a = StateT OctogulfState IO a

data Value = ValueInteger Integer | ValueString String | ValueNULL
  deriving (Show, Eq, Read, Ord)

data Procedure = Procedure {
  procName :: String,
  procArgs :: [String],
  procBody :: [Statement]
} deriving (Show, Eq, Read)

data Statement = Assignment String Statement | BinOp String Statement Statement | UniOp String Statement | VarRead String | 
                 Call String [Statement] | Map Statement [Statement] | NULL | IfElse Statement [Statement] [Statement] | If Statement [Statement] | Literal Value
  deriving (Show, Eq, Read)
