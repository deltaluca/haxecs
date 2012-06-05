module Main where

import System.Environment
import Parser
import Transform
import HaxePrinter

import Control.Monad.State
import Control.Monad.Writer

main = do
    args <- getArgs
    file <- parseFile (args !! 0)
    case(file) of
        Left err -> putStrLn $ "...................fail " ++ (show err)
        Right val -> do
            putStrLn $ printAST val
            putStrLn "-----"
            putStrLn $ printAST (transform val)
