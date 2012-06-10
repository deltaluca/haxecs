module Main where

import System.Environment
import Parser
import qualified ExprTransform as E
import qualified TypedefTransform as T
import HaxePrinter

import Control.Monad.State
import Control.Monad.Writer

main = do
    args <- getArgs
    file <- parseFile (head args)
    putStr (head args)
    let outfile = args !! 1
    case file of
        Left err -> putStrLn $ " :: ..................fail " ++ show err
        Right val
           -> do { let eval = E.transform val
                 ; let tval = T.transform eval
                 ; putStrLn " :: success"
                 }
