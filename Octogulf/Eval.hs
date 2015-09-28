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
  (global, local, procs) <- get
  put (global, ht : local, procs)

popScope :: Octomonad ()
popScope = do
  (global, (l:ls), procs) <- get
  put (global, ls, procs)

getGlobalScope :: Octomonad (HashTable String Value)
getGlobalScope = do
  (global, _, _) <- get
  return global


getLocalScopes :: Octomonad [(HashTable String Value)]
getLocalScopes = do
  (_, local, _) <- get
  return local


getLowestScope :: Octomonad (HashTable String Value)
getLowestScope = do
  (_, (x : xs), _) <- get
  return x

getProcs :: Octomonad (HashTable String Procedure)
getProcs = do
  (_, _, procs) <- get
  return procs


findProc :: String -> Octomonad (Maybe Procedure)
findProc name = do
  procs <- getProcs
  pv <- liftIO $ H.lookup procs name
  return pv


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
     r <- evalBlock $ procBody proc
     popScope
     return r


evalBlock :: [Statement] -> Octomonad Value
evalBlock [] = return ValueNULL
evalBlock [x] = evalStatement x
evalBlock (x:xs) = do
  evalStatement x
  evalBlock xs


evalStatement :: Statement -> Octomonad Value
evalStatement (Literal value) = return value

evalStatement NULL = return ValueNULL

evalStatement (VarRead var) = readVar var

evalStatement (Assignment var stmt) = do
  stmt' <- evalStatement stmt
  setVar var stmt'
  return stmt'

evalStatement (BinOp "+" left right) = do
  left' <- evalStatement left
  right' <- evalStatement right
  return $ binAdd left' right'

evalStatement (Call name args) = do
  args' <- mapM evalStatement args
  proc <- findProc name
  evalProcedure (fromJust proc) args'


binAdd (ValueString left) (ValueString right) = ValueString $ left ++ right
binAdd (ValueInteger left) (ValueInteger right) = ValueInteger $ left + right


insertArgs :: [(String, Value)] -> Octomonad ()
insertArgs [] = return ()
insertArgs ((k,v):xs) = do
  setVar k v
  insertArgs xs


runProcedure str = do
  hg <- newHT
  hp <- H.new
  let proc = runParserWithString parseProcedure str
  let initState = (hg, [], hp)
  evalStateT (evalProcedure proc [ValueString "This is stdin!"]) initState

runProgram str = do
  hg <- newHT
  let prog = runParserWithString parseProgram str
  hp <- H.new
  let initState = (hg, [], hp)
  insertProcs hp prog
  main <- H.lookup hp "Main"
  evalStateT (evalProcedure (fromJust main) []) initState

insertProcs :: HashTable String Procedure -> [Procedure] -> IO ()
insertProcs hp [] = return ()
insertProcs hp (x : xs) = do
  H.insert hp (procName x) x
  insertProcs hp xs
