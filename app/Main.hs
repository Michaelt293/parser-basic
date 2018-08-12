module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  (str:_) <- getArgs
  print $ runParser coinageMetal str
