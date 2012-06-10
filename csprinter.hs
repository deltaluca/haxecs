module CSPrinter (
        printAST -- :: File -> String
    ) where

import Parser
import Printer
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W (tell)
import Control.Monad.State
import Control.Arrow
import Data.List

printAST file = snd $ runWriter (runStateT (writeAST file) (True,0)) 

writeAST ("",traits) = mapM_ tellFTrait traits
writeAST (pckg,traits) = do
    tell $ "namespace " ++ pckg
    openScope
    mapM_ tellFTrait traits
    closeScope

tellFTrait (FImport pckg) = tellLn $ "using " ++ pckg ++ ";"
tellFTrait (FPre pre) = tellPre (mapM_ tellFTrait) pre >> nl
tellFTrait (FClass x s traits) = do
    tell "class "; tellType x
    maybeTell ((tell " : " >>) . tellType) s
    tell " "; openScope
    mapM_ tellCTrait traits
    closeScope

tellCTrait (CPre pre) = tellPre (mapM_ tellCTrait) pre
tellCTrait (CTrait as tinfo) = do
    mapM_ ((>> tell " ") . tellMember) as
--    tellTInfo tinfo
    where
        tellMember x = case x of
            APublic -> tell "public"
            APrivate -> tell "private"
            AStatic -> tell "static"
            AOverride -> tell "override"
            APre pre -> tellPre (mapM_ tellMember) pre
