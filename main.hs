module Main where

import System.Environment
import Parser

main = do { args <- getArgs
          ; file <- parseFile (args !! 0)
          ; case(file) of
                 Left err -> putStrLn $ "...................fail " ++ (show err)
                 Right _ -> putStrLn "success"
--                Left err -> putStrLn $ "Parser error: " ++ (show err)
--                Right err -> return ()
          }
