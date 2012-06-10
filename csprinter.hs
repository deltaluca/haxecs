module CSPrinter (
        printAST -- :: File -> String
    ) where

import Parser
import Printer hiding (tellType)
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

-----------------------------------------------------

tellFTrait (FImport pckg) = tellLn $ "using " ++ pckg ++ ";"
tellFTrait (FPre pre) = tellPre (mapM_ tellFTrait) pre >> nl
tellFTrait (FClass x s traits) = do
    tell "class "; tellType x
    maybeTell ((tell " : " >>) . tellType) s
    tell " "; openScope
    mapM_ tellCTrait traits
    closeScope

-----------------------------------------------------

tellCTrait (CPre pre) = tellPre (mapM_ tellCTrait) pre
tellCTrait (CTrait as tinfo) = do
    mapM_ ((>> tell " ") . tellMember) as
    tellTInfo tinfo
    where
        tellMember x = case x of
            APublic -> tell "public"
            APrivate -> tell "private"
            AStatic -> tell "static"
            AOverride -> tell "override"
            APre pre -> tellPre (mapM_ tellMember) pre

-----------------------------------------------------

tellTInfo (Member (x,Just t,e)) = do 
    tellType t; tell " "; tell x
    maybeTell ((tell " = " >>) . tellExpr) e
    tellLn ";"
tellTInfo (Member (x,Nothing,e))
    = error $ "Error: C# Requires member vars to be explicitly typed :: "
           ++ x ++ " = " ++ show e
tellTInfo (Member var) = tell "{member}" >> nl
tellTInfo (Method n (ps,Nothing,e))
    = error $ "Error: C# Requires functions to be explicitly typed :: "
           ++ n ++ " " ++ show ps ++ " " ++ show e
tellTInfo (Method n (ps,Just t,e)) = do
    tellType t; tell " "; tell n; tell "("
    sequence_ $ intersperse (tell ", ") $ map tellParam ps 
    tell ") { unchecked "
    tellExpr $ EBlock [e]
    tellLn "}"
tellTInfo (Property n g s t) = tell "{property}"

tellParam (True,(n,Just t,Nothing)) = do
    tellType t; tell " "; tell n; tell "= null"
tellParam (_,(n,Just t,Just e)) = do
    tellType t; tell " "; tell n; tell "="; tellExpr e
tellParam x = error "Error: C# Requires explicitly typed parameters"

-----------------------------------------------------

tellType (BasicType "Int") = tell "int"
tellType (BasicType "Float") = tell "double"
tellType (BasicType "Void") = tell "void"

tellType (BasicType x) = tell x

tellType (ParamType x xs)
    = do { tellType x; tell "<"; mapM_ tellType xs; tell ">" }
tellType (PreType pre) = tellPre tellType pre
tellType (FuncType x y)
    = do { tellType x; tell "->"; tellType y }

-----------------------------------------------------

tellExpr (EConst (CInt x)) = tell $ show x
tellExpr (EConst (CFloat x)) = tell $ show x
tellExpr (EConst (CString x)) = tell x
tellExpr (EConst (CIdent x)) = tell x

tellExpr (EArray xs) = do
    tell "(new[] {"
    sequence_ $ intersperse (tell ", ") $ map tellExpr xs
    tell "})"

tellExpr (EBlock xs) = do
    openScope
    mapM_ tellState xs
    closeScope
    where tellState (EPreN pre) = tellExpr (EPreN pre) >> nl
          tellState x = (>> tellLn ";") . tellExpr $ x

tellExpr (EVars xs) = mapM_ tellVar xs
    where
        tellVar (n,Just t,e) = do 
            tellType t; tell " "; tell n
            maybeTell ((tell " = " >>) . tellExpr) e
        tellVar (n,Nothing,Just e) = do
            tell "var "; tell n    
            tell " = "; tellExpr e
        tellVar x = error "Error: C# Requires explicitly typed, non-defined local vars"

tellExpr (EArrayAccess x y) = do { tellExpr x; tell "["; tellExpr y; tell "]" }
tellExpr (EUnop op flag e) = tell "(" >> (case flag of
    FlagPre  -> tell (showUnop op) >> tellExpr e
    FlagPost -> tellExpr e >> tell (showUnop op) ) >> tell ")"
tellExpr (EBinop op x y) = do { tell "("; tellExpr x; tell $ showOp op; tellExpr y; tell ")" }
tellExpr (ETernary x y z) = do { tell "("; tellExpr x; tell "?"; tellExpr y; tell ":"; tellExpr z; tell ")" }
tellExpr (EReturn x) = do
    tell "return "
    maybeTell tellExpr x

tellExpr (EStdFor x y z w) = do
    tell "for ("; tellExpr x; tell "; "; tellExpr y; tell "; "; tellExpr z; tell ") "
    tellExpr w

tellExpr (EContinue) = tell "continue"
tellExpr (EBreak) = tell "break"
tellExpr (EThrow x) = tell "throw " >> tellExpr x
tellExpr (EPre1 pre) = tellPre tellExpr pre

tellExpr (EPreN pre) = tellPre (mapM_ ((>> tellLn ";") . tellExpr)) pre

tellExpr (ENew t xs) = do
    tell "(new "; tellType t; tell "("
    sequence_ $ intersperse (tell ", ") $ map tellExpr xs
    tell "))"
tellExpr (ECall x xs) = do
    tell "("; tellExpr x; tell "("
    sequence_ $ intersperse (tell ", ") $ map tellExpr xs
    tell "))"
tellExpr (EField x i) = tellExpr x >> tell ('.' : i)
tellExpr (EWhile True x y) = do { tell "while("; tellExpr x; tell ")"; tellExpr y }
tellExpr (EWhile False x y) = do { tell "do "; tellExpr y; tell " while("; tellExpr x; tell ")" }

tellExpr (EIf x y z) = do
    tell "if("; tellExpr x; tell ") "; tellExpr y; tell " "
    maybeTell ((tell "else " >>) . tellExpr) z

tellExpr (EFunction (ps,_,e)) = do
    tell "(("; mapM_ param ps; tell ") => { unchecked "; tellExpr $ EBlock [e]; tell "})"
    where param (False,(i,Nothing,Nothing)) = tell i
          param (False,(i,Just t, Nothing)) = tellType t >> tell " " >> tell i

tellExpr e = tell "{expr}"
