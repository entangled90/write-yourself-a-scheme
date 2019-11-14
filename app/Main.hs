module Main where

import           Eval
import           Control.Monad.Except


main :: IO ()
main = do
  result <- runExceptT runRepl
  case result of
    Right _   -> pure ()
    Left  err -> print err
