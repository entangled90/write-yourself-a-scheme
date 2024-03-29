{-# LANGUAGE RecordWildCards #-}

module LispVal where
import           Control.Monad.Except
import           Text.ParserCombinators.Parsec  ( ParseError )
import qualified Data.Map                      as M
import           Data.IORef


type VarName = String
type ArgName = String
type Env = IORef (M.Map VarName (IORef LispVal))
type LispFunction = [LispVal] -> IOResult LispVal
type IOResult = ExceptT LispError IO

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                | Func { name:: Maybe String, args :: [ArgName], expr:: [LispVal], env:: Env}
                | PrimitiveFunc LispFunction

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom   atom    ) = atom
  show (Number contents) = show contents
  show (Bool   True    ) = "#t"
  show (Bool   False   ) = "#f"
  show (List   contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t ) = "(" ++ unwordsList h ++ ". " ++ show t ++ ")"
  show Func {..} =
    "lambda(" ++ show name ++ "), params: (" ++ unwords (map show args)
  show (PrimitiveFunc _) = "<primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

data LispError =
    NumArgs Integer [ LispVal ]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show (UnboundVar     message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form   ) = message ++ ": " ++ show form
  show (NotFunction    message func   ) = message ++ ": " ++ show func
  show (NumArgs expected found) =
    "Expected "
      ++ show expected
      ++ " args: found values ["
      ++ unwordsList found
      ++ "]"
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser  parseErr) = "Parse error at " ++ show parseErr
  show (Default str     ) = "Default: " ++ str
