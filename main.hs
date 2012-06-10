module Main where

import System.Environment
import Parser
import qualified ExprTransform as E
import qualified TypedefTransform as T
import qualified ForTransform as F
import qualified CSPrinter as CS
import qualified HaxePrinter as Haxe

import Control.Monad.State
import Control.Monad.Writer

main = do
    args <- getArgs
    file <- parseFile (head args)
    let outfile = args !! 1
    case file of
        Left err -> putStrLn $ " :: ..................fail " ++ show err
        Right val
           -> do { let eval = E.transform val
                 ; putStrLn $ show eval
                 ; putStrLn $ Haxe.printAST eval
                 ; let tval = T.transform eval
                 ; putStrLn $ show tval
                 ; putStrLn $ Haxe.printAST tval
                 ; let fval = F.transform tval
                 ; putStrLn $ show fval
                 ; putStrLn $ CS.printAST fval
                 }
