module Main where

import           Lib
import           Parser
import           LispVal
import           Eval
import           System.Environment


main :: IO ()
main = do
  script : _ <- getArgs
  case parse script of
    Right parsed -> print $ show $ eval parsed
    Left  err    -> print err
