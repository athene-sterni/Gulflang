module Octogulf.Parser (
   runParserWithString,
   parseProcedure,
   parseProgram
  ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

import Data.Maybe
import Debug.Trace

import Octogulf.Types


import qualified Data.HashTable.IO as H

myTrace a b = b -- trace a b
myShowTrace q = q -- trace (show q) q

skipAhead = do
  many1 (skipSpace <|> skipComment)
  return ()


skipSpace :: Parser ()
skipSpace = do
  many1 $ oneOf " \n\r\t"
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
            do { char '@'; return "@"; })
  optional skipAhead
  return . myShowTrace $ ident


parseProcedure :: Parser Procedure
parseProcedure = do
  ident <- parseIdentifier
  args <- parseArgs
  body <- parseBlock
  return $ Procedure { procName = ident, procArgs = args, procBody = body }


parseProgram :: Parser [Procedure]
parseProgram = do
  many1 $ parseProcedure


parseBlock = myTrace "parseBlock" $ do
  parseChar '{'
  statements <- many parseStatement
  parseChar '}'
  return . myShowTrace $ statements


parseStatement = myTrace "parseStatement" $ do
  optional $ skipAhead
  stmt <- (try parseStr) <|> (try parseInt) <|> (try parseIfElse) <|> (try parseIf) <|> (try parseMap) <|> (try parseAssignment) <|> (try parseBinOp) 
          <|> parseUniOp <|> (try parseCall) <|> parseVarRead <|> (parseChar 'q' >> return NULL)
  optional $ skipAhead
  return . myShowTrace $ stmt


parseMap = myTrace "parseMap" $ do
  parseString "map"
  val <- parseStatement
  stmt <- parseBlock
  return . myShowTrace $ Map val stmt


parseIfElse = myTrace "parseIfElse" $ do
  parseString "if"
  cond <- parseStatement
  ifblock <- parseBlock
  parseString "else"
  elseblock <- parseBlock
  return . myShowTrace $ IfElse cond ifblock elseblock


parseIf = myTrace "parseIf" $ do
  parseString "if"
  cond <- parseStatement
  ifblock <- parseBlock
  return . myShowTrace $ If cond ifblock


parseAssignment = myTrace "parseAssignment" $ do
  parseChar ':'
  ident <- parseIdentifier
  stmt <- parseStatement
  return . myShowTrace $ Assignment ident stmt


parseBinOp = myTrace "parseBinOp" $ (do
  op <- choice $ map parseString ["+","<",">"]
  a <- parseStatement
  b <- parseStatement
  return . myShowTrace $ BinOp op a b)

foldM f [x] = return x
foldM f (x:xs) = do
  rest <- foldM f xs
  x `f` rest


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


parseInt = myTrace "parseInt" $ do
  optional $ skipAhead
  sign <- optionMaybe $ char '_'
  dgs <- many1 $ oneOf ['0'..'9']
  optional $ skipAhead
  if isJust sign
    then return . myShowTrace $ Literal (ValueInteger ( (-1) * (read dgs) ))
    else return . myShowTrace $ Literal (ValueInteger (read dgs))


parseStr = do
  optional $ skipAhead
  char '"'
  chars <- many $ noneOf "\""
  char '"'
  optional $ skipAhead
  return $ Literal (ValueString chars)


runParserWithString p input = 
  case parse p "" input of
    Left err -> error $ show err
    Right q -> q

