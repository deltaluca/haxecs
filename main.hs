module Main where

import System.Environment
import Parser

main = do { args <- getArgs
          ; file <- parseFile (args !! 0)
          ; case(file) of
                 Left err -> putStrLn $ "...................fail " ++ (show err)
                 Right val -> putStrLn $ show val
          }
