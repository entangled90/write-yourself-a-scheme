module Eval where

import           LispVal
import qualified Data.Map                      as M
import           Control.Monad.Except

import           System.IO               hiding ( try )
import           LispVal
import           Parser
import           Data.IORef
import qualified Data.Map                      as M
import           Control.Monad.Except
import           Kernel

eval :: Env -> LispVal -> IOResult LispVal
eval env (Atom var                         ) = getVar env var
eval env (List [Atom "quote", v]           ) = pure v

eval env (List [Atom "if", p, thenF, elseF]) = do
  result <- eval env p
  bool   <- unpackBool result
  if bool then eval env thenF else eval env elseF

eval env (List [Atom "define!", Atom var, form]) =
  eval env form >>= defineVar env var

eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var

eval env (List (Atom func : args)) =
  traverse (eval env) args >>= apply env func
eval env v = pure v

apply :: Env -> FuncName -> [LispVal] -> IOResult LispVal
apply env name args = case M.lookup name (primitives env) of
  Just f  -> f args
  Nothing -> throwError $ NotFunction name (foldMap show args)

primitives :: Env -> M.Map String LispFunction
primitives env =
  M.fromList $ numericOps ++ typeChecks ++ boolOps ++ strOps ++ kernelOps

numericOps :: [(String, LispFunction)]
numericOps =
  [ ("+"   , numericBinop (+))
  , ("-"   , numericBinop (-))
  , ("*"   , numericBinop (*))
  , ("mod" , numericBinop mod)
  , ("quot", numericBinop quot)
  , ("rem" , numericBinop rem)
  , ("=="  , numBoolBinOp (==))
  , ("<"   , numBoolBinOp (<))
  , (">"   , numBoolBinOp (>))
  , ("/="  , numBoolBinOp (/=))
  , (">="  , numBoolBinOp (>=))
  , ("<="  , numBoolBinOp (<=))
  ]

boolOps = [("&&", boolBoolBinOp (&&)), ("||", boolBoolBinOp (||))]
strOps =
  [ ("string=?" , strBoolBinOp (==))
  , ("string?"  , strBoolBinOp (>))
  , ("string<=?", strBoolBinOp (<=))
  , ("string>=?", strBoolBinOp (>=))
  ]


typeChecks =
  [ ("string?", isString)
  , ("number?", isNumber)
  , ("bool?"  , isBool)
  , ("list?"  , isList)
  ]

weakOps = []

isString :: LispFunction
isString ([String _]) = pure $ Bool True
isString ([_       ]) = pure $ Bool False
isString args         = throwError $ NumArgs 1 args


isNumber :: LispFunction
isNumber ([Number _]) = pure $ Bool True
isNumber ([_       ]) = pure $ Bool False
isNumber args         = throwError $ NumArgs 1 args


isBool :: LispFunction
isBool ([Bool _]) = pure $ Bool True
isBool ([_     ]) = pure $ Bool False
isBool args       = throwError $ NumArgs 1 args

isList :: LispFunction
isList ([List _]) = pure $ Bool True
isList ([_     ]) = pure $ Bool False
isList args       = throwError $ NumArgs 1 args


numericBinop :: (Integer -> Integer -> Integer) -> LispFunction
numericBinop op ([p1, p2]) = do
  arg1 <- unpackNum p1
  arg2 <- unpackNum p2
  pure $ Number $ op arg1 arg2
numericBinop op args = throwError $ NumArgs 2 args


--------  REPL  -------


flush :: String -> IOResult ()
flush a = liftIO $ putStrLn a >> hFlush stdout

readPrompt :: String -> IO String
readPrompt a = putStr a >> hFlush stdout >> getLine

evalAndPrint :: Env -> String -> IOResult ()
evalAndPrint env s = evalString env s >>= flush

evalString :: Env -> String -> IOResult String
evalString env script = do
  parsed <- parse script
  (show <$> (eval env parsed)) `catchError` (pure . show)

runRepl :: IOResult ()
runRepl = do
  env <- liftIO emptyEnv
  until_ (\i -> i == ":q") (liftIO $ readPrompt "Lisp>>>") (evalAndPrint env)


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  result <- prompt
  if (p result) then pure () else action result >> until_ p prompt action



----- Environmentn

emptyEnv :: IO Env
emptyEnv = newIORef M.empty


isBound :: Env -> VarName -> IO Bool
isBound env name = M.member name <$> readIORef env

getVar :: Env -> VarName -> IOResult LispVal
getVar env name = do
  map <- liftIO $ readIORef env
  case M.lookup name map of
    Just lispVal -> liftIO $ readIORef lispVal
    Nothing      -> throwError $ UnboundVar "Getting an unbound variable" name


setVar :: Env -> VarName -> LispVal -> IOResult LispVal
setVar env name value = do
  map <- liftIO $ readIORef env
  case M.lookup name map of
    Just ioRef -> const value <$> (liftIO $ writeIORef ioRef value)
    Nothing -> throwError $ UnboundVar "Trying to set an unbound variable" name


defineVar :: Env -> VarName -> LispVal -> IOResult LispVal
defineVar env name value = do
  map <- liftIO $ readIORef env
  case M.lookup name map of
    Just ioRef -> setVar env name value
    Nothing    -> liftIO $ do
      newRef <- newIORef value
      writeIORef env (M.insert name newRef map)
      pure $ value
