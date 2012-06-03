module Main where

import System.Environment
import Parser

main = do { args <- getArgs
          ; putStrLn $ "parsing "++(args!!0)
          ; file <- parseFile (args !! 0)
          ; putStrLn $ show file
          }
