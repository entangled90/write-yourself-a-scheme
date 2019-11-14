module Parser where

import           Control.Monad
import           Control.Monad.Except
import           System.Environment
import           Text.ParserCombinators.Parsec as P
                                         hiding ( spaces )
import           LispVal

parse :: String -> ThrowsError LispVal
parse input = case P.parse parseExpr "string" input of
  Right result -> pure result
  Left  error  -> throwError $ Parser error

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of
    "#t"      -> Bool True
    "#f"      -> Bool False
    otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [quote, x]

parseLists :: Parser LispVal
parseLists = do
  char '('
  x <- (try parseList) <|> parseDottedList
  char ')'
  return x

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseLists

quote :: LispVal
quote = Atom "quote"
