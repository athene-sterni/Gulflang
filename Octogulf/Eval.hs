module Octogulf.Eval (
  ) where

import Octogulf.Types
import Octogulf.Parser

import qualified Data.HashTable.IO as H
import Control.Monad
import Control.Monad.State
import System.IO
import Control.Monad.IO.Class
import Data.Maybe

newHT :: IO (HashTable String Value)
newHT = H.new


newScope :: Octomonad ()
newScope = do
  ht <- liftIO $ newHT
  pushScope (ht)
  return ()


pushScope :: HashTable String Value -> Octomonad ()
pushScope ht = do
  (global, local) <- get
  put (global, ht : local)

getGlobalScope :: Octomonad (HashTable String Value)
getGlobalScope = do
  (global, _) <- get
  return global


getLocalScopes :: Octomonad [(HashTable String Value)]
getLocalScopes = do
  (_, local) <- get
  return local


getLowestScope :: Octomonad (HashTable String Value)
getLowestScope = do
  (_, (x : xs)) <- get
  return x


findVar :: String -> Octomonad (Maybe (HashTable String Value, Value))
findVar var = do
  global <- getGlobalScope
  local <- getLocalScopes
  gv <- liftIO $ H.lookup global var
  if isJust gv
   then return . Just $ (global, fromJust $ gv)
   else findVar' var local
 where findVar' var [] = return Nothing
       findVar' var (x:xs) = do
         lv <- liftIO $ H.lookup x var
         if isJust lv
           then return . Just $ (x, fromJust $ lv)
           else findVar' var xs


readVar :: String -> Octomonad (Value)
readVar var = do
  r <- findVar var
  case r of
    (Just (_, v)) -> return v
    _ -> return ValueNULL


setVar :: String -> Value -> Octomonad ()
setVar var val = do
  r <- findVar var
  case r of
    (Just (ht, _)) -> do
      liftIO $ H.insert ht var val
      return ()
    _ -> do
      ht <- getLowestScope
      liftIO $ H.insert ht var val
      return ()


evalProcedure :: Procedure -> [Value] -> Octomonad Value
evalProcedure proc args = do
  if length args /= length (procArgs proc)
   then error "Invalid number of arguments!"
   else do
     newScope
     insertArgs $ zip (procArgs proc) args
     evalBlock $ procBody proc


evalBlock :: [Statement] -> Octomonad Value
evalBlock [] = return ValueNULL
evalBlock [x] = evalStatement x
evalBlock (x:xs) = do
  evalStatement x
  evalBlock xs


evalStatement :: Statement -> Octomonad Value
evalStatement NULL = return ValueNULL

evalStatement (VarRead var) = readVar var


insertArgs :: [(String, Value)] -> Octomonad ()
insertArgs [] = return ()
insertArgs ((k,v):xs) = do
  setVar k v
  insertArgs xs


runProcedure str = do
  hg <- newHT
  let proc = runParserWithString parseProcedure str
  let initState = (hg, [])
  evalStateT (evalProcedure proc [ValueString "This is stdin!"]) initState
