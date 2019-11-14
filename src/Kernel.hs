{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}

module Kernel where

import           LispVal
import           Control.Monad.Except

type FuncName = String

kernelOps :: [(FuncName, LispFunction)]
kernelOps = [("eqv?", eqv), ("equals?", weakEquals)]


eqv :: LispFunction
eqv [String s1, String s2] = pure $ Bool $ s1 == s2
eqv [Number s1, Number s2] = pure $ Bool $ s1 == s2
eqv [List   s1, List s2  ] = (Bool) <$> (and <$> traverse eqvPair (zip s1 s2))
  where eqvPair (a1, a2) = eqv [a1, a2] >>= unpackBool
eqv [Bool s1, Bool s2] = pure $ Bool $ s1 == s2
eqv [(DottedList s1 d1), (DottedList s2 d2)] =
  eqv [List (s1 ++ [d1]), List (s2 ++ [d2])]
eqv [Atom a1, Atom a2] = pure $ Bool $ a1 == a2
eqv [_      , _      ] = pure $ Bool False
eqv badArgs            = throwError $ NumArgs 2 badArgs

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> IOResult a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> IOResult Bool
unpackEquals v1 v2 (AnyUnpacker unpacker) =
  let equals = do
        u1 <- unpacker v1
        u2 <- unpacker v2
        pure $ (u1 == u2)
  in  equals `catchError` (const $ pure False)


weakEquals :: LispFunction
weakEquals args@[arg1, arg2] = do
  let unpackers =
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  primitiveEquals <- or <$> traverse (unpackEquals arg1 arg2) unpackers
  eqvEquals       <- eqv args
  pure
    $  Bool
    $  primitiveEquals
    || (case eqvEquals of
         Bool bool -> bool
         _         -> False
       )
weakEquals args = throwError $ NumArgs 2 args





unpackNum :: LispVal -> IOResult Integer
unpackNum (  Number n) = pure n
unpackNum s@(String n) = case reads n of
  (h_int, _) : _ -> pure h_int
  _              -> throwError $ TypeMismatch "is not a number" s
unpackNum n = throwError
  $ TypeMismatch "it should be something that can be converted to a number" n

unpackStr :: LispVal -> IOResult String
unpackStr (String s) = pure s
unpackStr arg        = throwError $ TypeMismatch "expected a string" arg

unpackBool :: LispVal -> IOResult Bool
unpackBool (Bool b) = pure b
unpackBool arg      = throwError $ TypeMismatch "expected a boolean" arg

numBoolBinOp = boolBinOp unpackNum
strBoolBinOp = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool


boolBinOp :: (LispVal -> IOResult a) -> (a -> a -> Bool) -> LispFunction
boolBinOp convert op ([p1, p2]) = do
  a1 <- convert p1
  a2 <- convert p2
  pure $ Bool $ op a1 a2
