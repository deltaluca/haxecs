module CSPrinter (
        printAST -- :: File -> String
    ) where

import Parser
import Printer hiding (tellType,tellPre)
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W (tell)
import Control.Monad.State
import Control.Arrow
import Data.List

printAST file = snd $ runWriter (runStateT (writeAST file) (True,0)) 

writeAST ("",traits) = do
    -- injected import
    tellLn "using System;"

    mapM_ tellFTrait traits

writeAST (pckg,traits) = do
    tell $ "namespace " ++ pckg
    openScope

    -- injected import
    tellLn "using System;"

    mapM_ tellFTrait traits
    closeScope

-----------------------------------------------------

tellPre :: (a -> StateT (Bool,Int) (Writer String) ()) -> Pre a -> StateT (Bool,Int) (Writer String) ()
tellPre teller (cond, ife, elses, elsee) = do
    maybeNl
    tell "#if "; tellCond cond; nl; teller ife; nl;
    mapM_ tellElse elses
    maybeTell ((tellLn "#else" >>) . teller) elsee; nl;
    tellLn "#endif"
    where
        tellElse (cond, ife)
            = do { tell "#elif "; tellCond cond; nl; teller ife; nl }

        tellCond (PreIdent "haxecs") = tell "true"
        tellCond (PreIdent x) = tell x

        tellCond (PreAnd a b) = do { tell "("; tellCond a; tell "&&"; tellCond b; tell ")" }
        tellCond (PreOr a b) = do { tell "("; tellCond a; tell "||"; tellCond b; tell ")" }
        tellCond (PreNot a) = do { tell "(!"; tellCond a; tell ")" }

-----------------------------------------------------

tellFTrait (FImport pckg) = tellLn $ "using " ++ pckg ++ ";"
tellFTrait (FPre pre) = tellPre (mapM_ tellFTrait) pre >> nl
tellFTrait (FClass x s traits) = do
    tell "class "; tellType x
    maybeTell ((tell " : " >>) . tellType) s
    tell " "; openScope
    mapM_ (tellCTrait x) traits

    -- injected method for lambdas to use
    tellLn "// injected for lambda type inference and inline placement issues in C#"
    tellLn "private static T __call__<T>(Func<T> x) { return x(); }"
    closeScope

-----------------------------------------------------

tellCTrait cname (CPre pre) = tellPre (mapM_ (tellCTrait cname)) pre
tellCTrait cname (CTrait as tinfo) = do
    mapM_ ((>> tell " ") . tellMember) as
    tellTInfo cname tinfo
    where
        tellMember x = case x of
            APublic -> tell "public"
            APrivate -> tell "private"
            AStatic -> tell "static"
            AOverride -> tell "override"
            AInline -> return ()
            APre pre -> tellPre (mapM_ tellMember) pre

-----------------------------------------------------

tellTInfo _ (Member (x,Just t,e)) = do 
    tellType t; tell " "; tell x
    maybeTell ((tell " = " >>) . tellExpr) e
    tellLn ";"
tellTInfo _ (Member (x,Nothing,e))
    = error $ "Error: C# Requires member vars to be explicitly typed :: "
           ++ x ++ " = " ++ show e
tellTInfo _ (Member var) = tell "{member}" >> nl

tellTInfo cname (Method "new" (ps,_,e)) = do
    tellType cname; tell "("
    sequence_ $ intersperse (tell ", ") $ map tellParam ps 
    tell ") { unchecked "
    tellExpr $ EBlock [e]
    tellLn "}"
tellTInfo _ (Method n (ps,Nothing,e))
    = error $ "Error: C# Requires functions to be explicitly typed :: "
           ++ n ++ " " ++ show ps ++ " " ++ show e
tellTInfo _ (Method n (ps,Just t,e)) = do
    tellType t; tell " "; tell n; tell "("
    sequence_ $ intersperse (tell ", ") $ map tellParam ps 
    tell ") { unchecked "
    tellExpr $ EBlock [e]
    tellLn "}"

tellTInfo _ (Property n g s t) = tell "{property}"

tellParam (True,(n,Just t,Nothing)) = do
    tellType t; tell " "; tell n; tell "= null"
tellParam (False,(n,Just t,Nothing)) = do
    tellType t; tell " "; tell n;
tellParam (_,(n,Just t,Just e)) = do
    tellType t; tell " "; tell n; tell "="; tellExpr e

tellParam x = error $ "Error: C# Requires explicitly typed parameters :: " ++ (show x)

-----------------------------------------------------

tellType (BasicType "Int") = tell "int"
tellType (BasicType "Float") = tell "double"
tellType (BasicType "Void") = tell "void"
tellType (BasicType "String") = tell "string"
tellType (BasicType "Bool") = tell "bool"
tellType (BasicType x) = tell x

tellType (ParamType (BasicType "Array") (x:[]))
    = do { tellType x; tell "[]" } 
tellType (ParamType x xs)
    = do { tellType x; tell "<"; mapM_ tellType xs; tell ">" }

tellType (PreType pre) = tellPre tellType pre

tellType (FuncType x y)
    = do { tell "Func<"
         ; sequence_ $ intersperse (tell ", ") $ map tellType xs
         ; tell ">" }
    where xs = args (FuncType x y)
          args (FuncType (BasicType "Void") (FuncType y z)) = args (FuncType y z)
          args (FuncType x (FuncType y z)) = x : args (FuncType y z)
          args (FuncType (BasicType "Void") y) = [y]
          args (FuncType x y) = [x,y]

-----------------------------------------------------

btellExpr x = tell "(" >> tellExpr x >> tell ")"

tellExpr (EConst (CInt x)) = tell $ show x
tellExpr (EConst (CFloat x)) = tell $ show x
tellExpr (EConst (CString x)) = tell x

tellExpr (EConst (CIdent "trace")) = tell "Console.WriteLine"
tellExpr (EConst (CIdent x)) = tell x

tellExpr (EArray xs) = do
    tell "new[] {"
    sequence_ $ intersperse (tell ", ") $ map tellExpr xs
    tell "}"

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
            maybeTell ((tell " = " >>) . btellExpr) e
        tellVar (n,Nothing,Just e) = do
            tell "var "; tell n    
            tell " = "; btellExpr e
        tellVar x = error "Error: C# Requires explicitly typed, non-defined local vars"

tellExpr (EArrayAccess x y) = do { btellExpr x; tell "["; btellExpr y; tell "]" }
tellExpr (EUnop op flag e) = case flag of
    FlagPre  -> tell (showUnop op) >> btellExpr e
    FlagPost -> btellExpr e >> tell (showUnop op)
tellExpr (EBinop op x y) = do { btellExpr x; tell $ showOp op; btellExpr y; }
tellExpr (ETernary x y z) = do { btellExpr x; tell "?"; btellExpr y; tell ":"; btellExpr z; }
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
    tell "new "; tellType t; tell "("
    sequence_ $ intersperse (tell ", ") $ map tellExpr xs
    tell ")"
tellExpr (ECall x xs) = do
    tellExpr x; tell "("
    sequence_ $ intersperse (tell ", ") $ map btellExpr xs
    tell ")"
tellExpr (EField x i) = tellExpr x >> tell ('.' : i)
tellExpr (EWhile True x y) = do { tell "while("; tellExpr x; tell ")"; tellExpr y }
tellExpr (EWhile False x y) = do { tell "do "; tellExpr y; tell " while("; tellExpr x; tell ")" }

tellExpr (EIf x y z) = do
    tell "if("; tellExpr x; tell ") "; tellExpr y; tell " "
    maybeTell ((tell "else " >>) . tellExpr) z

tellExpr (EFunction (ps,_,e)) = do
    tell "("; mapM_ param ps; tell ") => "; tellExpr e
    where param (False,(i,Nothing,Nothing)) = tell i
          param (False,(i,Just t, Nothing)) = tellType t >> tell " " >> tell i

tellExpr e = tell "{expr}"
