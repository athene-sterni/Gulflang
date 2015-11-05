module Octogulf.Types (
   HashTable(..),
   OctogulfState(..),
   Octomonad(..),
   Value(..),
   Procedure(..),
   Statement(..),
   ObjMembers(..)
  ) where

import qualified Data.HashTable.IO as H
import Control.Monad.State

type HashTable k v = H.CuckooHashTable k v

type OctogulfState = (HashTable String Value, [HashTable String Value], HashTable String Procedure)
type Octomonad a = StateT OctogulfState IO a

data Value = ValueInteger Integer | ValueString String | ValueNULL | ValueObj ObjMembers | ValueProc Procedure
  deriving (Show, Eq, Read, Ord)

data ObjMembers = ObjMembers (HashTable String Value)

instance Show ObjMembers where
  show (ObjMembers ht) = show ht

instance Eq ObjMembers

instance Read ObjMembers

instance Ord ObjMembers
  

data Procedure = Procedure {
  procName :: String,
  procArgs :: [String],
  procBody :: [Statement]
} deriving (Show, Eq, Read, Ord)

data Statement = Assignment String Statement | BinOp String Statement Statement | UniOp String Statement | VarRead String | 
                 Call String [Statement] | Map Statement [Statement] | NULL | IfElse Statement [Statement] [Statement] | If Statement [Statement] | Literal Value |
                 Obj [(String, Statement)] | ObjCall Statement String | InvokeObj Statement String [Statement] | ObjAssignment Statement String Statement
  deriving (Show, Eq, Read, Ord)
