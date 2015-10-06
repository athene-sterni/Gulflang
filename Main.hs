import System.Environment
import System.Exit

import Octogulf.Eval

usage :: IO ()
usage = do
  putStrLn $ "og 0.0.1 - (c) 2015 -, Octogulf people"
  pn <- getProgName
  putStrLn $ pn ++ "\t--file <path>\t\tLoad and execute file."
  putStrLn $ "\t--prog <program>\tPass program as commmand line argument."
  putStrLn $ "\t--prog-main\t\tPass snippet as command line argument."
  putStrLn $ "\t\t \t\t(assume inside Main { })"
  exitWith (ExitFailure 2)
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--file", path] -> do
      txt <- readFile path
      runProgram txt
      exitSuccess
    ["--prog", program] -> do
      runProgram program
      exitSuccess
    ["--prog-main", snippet] -> do
      runProgram $ "Main {" ++ snippet ++ "}"
      exitSuccess
    ["--version"] -> usage
    ["--help"] -> usage
    _ -> usage
  