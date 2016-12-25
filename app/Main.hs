module Main where

import System.Environment
import Board

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ head args
