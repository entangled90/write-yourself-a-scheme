module Parser where

import           Control.Monad
import           Control.Monad.Except
import           Text.ParserCombinators.Parsec as P
                                         hiding ( spaces )
import           LispVal

parse :: String -> IOResult LispVal
parse input = case P.parse parseExpr "string" input of
  Right result -> pure result
  Left  err    -> throwError $ Parser err

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  void $ char '"'
  x <- many (noneOf "\"")
  void $ char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  void $ char '\''
  x <- parseExpr
  return $ List [quote, x]

parseLists :: Parser LispVal
parseLists = do
  void $ char '('
  x <- (try parseList) <|> parseDottedList
  void $ char ')'
  return x

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseLists

quote :: LispVal
quote = Atom "quote"
