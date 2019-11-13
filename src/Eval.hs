module Eval
  ( eval
  )
where

import           LispVal
import qualified Data.Map                      as M

eval :: LispVal -> LispVal
eval (List [Atom "quote", v] ) = v
eval (List (Atom func : args)) = apply func $ map eval args
eval v                         = v
-- eval v@(String _)             = v
-- eval v@(Number _)             = v
-- eval v@(Bool   _)             = v



apply :: String -> [LispVal] -> LispVal
apply func args = case M.lookup func primitives of
  Just f  -> f args
  Nothing -> error $ "function " ++ func ++ " not defined"


primitives :: M.Map String ([LispVal] -> LispVal)
primitives =
  M.fromList
    $ [ ("+"   , numericBinop (+))
      , ("-"   , numericBinop (-))
      , ("*"   , numericBinop (+))
      , ("mod" , numericBinop mod)
      , ("quot", numericBinop quot)
      , ("rem" , numericBinop rem)
      ]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = case reads n of
  (h_int, _) : _ -> h_int
  _              -> 0
unpackNum n = error $ "unsupported operation for " ++ show n
