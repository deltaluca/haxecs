module Main where

import System.Environment
import Parser
import qualified ExprTransform as E
import qualified TypedefTransform as T
import qualified CSTransform as F
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
           -> do { let tval = T.transform val
                 ; putStrLn "After typedef transforms"
                 ; print tval
                 ; putStrLn $ Haxe.printAST tval
                 ; let eval = E.transform tval
                 ; putStrLn "After expression transforms"
                 ; print eval
                 ; putStrLn $ CS.printAST eval
                 ; let fval = F.transform eval
                 ; putStrLn "After c# specific transforms"
                 ; print fval
                 ; putStrLn $ CS.printAST fval
                 ; writeFile outfile (CS.printAST fval)
                 }
