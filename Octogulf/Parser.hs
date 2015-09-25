import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import Data.Maybe
import Debug.Trace


import qualified Data.HashTable.IO as H

myTrace a b = trace a b
myShowTrace q = trace (show q) q


data Procedure = Procedure {
  procName :: String,
  procArgs :: [String],
  procBody :: [Statement]
} deriving (Show, Eq, Read)

data Statement = Assignment String Statement | BinOp String Statement Statement | UniOp String Statement | VarRead String | Call String [Statement] | Map Statement [Statement] | NULL
  deriving (Show, Eq, Read)

skipAhead = do
  many1 (skipSpace <|> skipComment)
  return ()

skipSpace :: Parser ()
skipSpace = do
  many1 $ oneOf " "
  return ()

skipComment :: Parser ()
skipComment = do
  string "\\="
  many1 $ noneOf "\\"
  string "\\"
  return ()

parseChar c' = do
  optional skipAhead
  c <- char c'
  optional skipAhead
  return c

parseString s' = do
  optional skipAhead
  s <- string s'
  optional skipAhead
  return s

parseArgs = do 
  optional skipAhead
  args <- many parseIdentifier
  optional skipAhead
  return args

parseIdentifier = myTrace "parseIdentifier" $ do
  optional skipAhead
  ident <- (do { char '$'; i <- many1 $ oneOf (['a'..'z']++['A'..'Z']); return i;} <|>
            do { c <- oneOf ['a'..'z']; return [c]; } <|>
            do { c <- oneOf ['A'..'Z']; i <- many1 $ oneOf (['a'..'z']++['A'..'Z']); return $ c:i;} <|>
            do { char '_'; return "_"; })
  optional skipAhead
  return . myShowTrace $ ident

parseProcedure :: Parser Procedure
parseProcedure = do
  ident <- parseIdentifier
  args <- parseArgs
  body <- parseBlock
  return $ Procedure { procName = ident, procArgs = args, procBody = body }

parseBlock = myTrace "parseBlock" $ do
  parseChar '{'
  statements <- many parseStatement
  parseChar '}'
  return . myShowTrace $ statements

parseStatement = myTrace "parseStatement" $ do
  stmt <- (try parseMap) <|> (try parseAssignment) <|> (try parseBinOp) <|> parseUniOp <|> (try parseCall) <|> parseVarRead <|> (parseChar 'q' >> return NULL)
  return . myShowTrace $ stmt

parseMap = myTrace "parseMap" $ do
  parseString "map"
  val <- parseStatement
  stmt <- parseBlock
  return . myShowTrace $ Map val stmt

parseAssignment = do
  parseChar ':'
  ident <- parseIdentifier
  stmt <- parseStatement
  return $ Assignment ident stmt

parseBinOp = (do
  op <- parseString "+" <|> parseString "-" <|> parseString "*"
  a <- parseStatement
  b <- parseStatement
  return $ BinOp op a b)

parseUniOp = (do
  op <- parseString "#+"
  a <- parseStatement
  return $ UniOp op a)

parseVarRead = do
  v <- parseIdentifier
  return $ VarRead v

parseCall = do
  ident <- parseIdentifier
  parseChar '('
  args <- many parseStatement
  parseChar ')'
  return $ Call ident args

runParserWithString p input = 
  case parse p "" input of
    Left err -> error $ show err
    Right q -> q

