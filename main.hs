module Main where

import System.Environment
import Parser

main = do { args <- getArgs
          ; file <- parseFile (args !! 0)
          ; putStrLn $ show file
          }
