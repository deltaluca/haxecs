module HaxePrinter (
        printAST -- :: File -> String
    ) where

import Parser
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W (tell)
import Control.Monad.State
import Control.Arrow
import Data.List

printAST file = snd $ runWriter (runStateT (writeAST file) (True,0)) 

writeAST (pckg,traits) = do
    tellLn $ "package " ++ pckg ++ ";"
    nl
    mapM_ tellFTrait traits

tellFTrait (FImport pckg) = tellLn $ "import " ++ pckg ++ ";"
tellFTrait (FTypeDef x y) = do { tell "typedef "; tellType x; tell "="; tellType y; tellLn ";" }
tellFTrait (FPre pre) = tellPre (mapM_ tellFTrait) pre >> nl
tellFTrait (FClass x s traits) = do 
    tell "class "; tellType x
    maybeTell ((tell " extends " >>) . tellType) s
    tell " "; openScope
    mapM_ tellCTrait traits
    closeScope

tellCTrait (CPre pre) = tellPre (mapM_ tellCTrait) pre
tellCTrait (CTrait as tinfo) = do
    mapM_ ((>> tell " ") . tellMember) as
    tellTInfo tinfo
    where
        tellMember x = case x of
            APublic   -> tell "public"
            APrivate  -> tell "private"
            AStatic   -> tell "static"
            AOverride -> tell "override"
            AInline   -> tell "inline"
            APre pre  -> (tellPre (mapM_ tellMember) pre)

tellTInfo (Member var) = do { tell "var "; tellVar var; tellLn ";" }
tellTInfo (Method n f) = do
    tell $ "function " ++ n
    tellFExpr f
tellTInfo (Property n g s t) = do
    tell $ "var " ++ n ++ "(" ++ g ++ "," ++ s ++ ")" ++ ":"
    tellType t
    tellLn ";"

tellFExpr (ps,t,e) = do
    tell "("
    sequence_ $ intersperse (tell ", ") $ map tellParam ps
    tell ")"
    maybeTell ((tell ":" >>) . tellType) t
    tellExpr e

tellParam (opt,var) = when opt (tell "?") >> tellVar var
tellVar (n,t,v) = do
    tell n
    maybeTell ((tell ":" >>) . tellType) t
    maybeTell ((tell " = " >>) . tellExpr) v 

-----------------------------------------------

tellExpr (EConst (CInt x)) = tell $ show x
tellExpr (EConst (CFloat x)) = tell $ show x
tellExpr (EConst (CString x)) = tell x
tellExpr (EConst (CIdent x)) = tell x

tellExpr (EArray xs) = do
    tell "["
    sequence_ $ intersperse (tell ", ") $ map tellExpr xs
    tell "]"
tellExpr (EBlock xs) = do
    openScope
    mapM_ ((>> tell ";") . tellExpr) xs
    closeScope
tellExpr (EVars vs) = do
    tell "var "
    sequence_ $ intersperse (tell ", ") $ map tellVar vs
tellExpr (EArrayAccess x y) = do { tellExpr x; tell "["; tellExpr y; tell "]" }
tellExpr (EUnop op flag e) = tell "(" >> (case flag of
    FlagPre  -> tell (showUnop op) >> tellExpr e
    FlagPost -> tellExpr e >> tell (showUnop op) ) >> tell ")"
tellExpr (EBinop op x y) = do { tell "("; tellExpr x; tell $ showOp op; tellExpr y; tell ")" }
tellExpr (ETernary x y z) = do { tell "("; tellExpr x; tell "?"; tellExpr y; tell ":"; tellExpr z; tell ")" }
tellExpr (EReturn x) = do
    tell "return "
    maybeTell tellExpr x
tellExpr (EIn x y) = do { tell "("; tell $ x ++ " in "; tellExpr y; tell ")" }
tellExpr (EContinue) = tell "continue"
tellExpr (EBreak) = tell "break"
tellExpr (EThrow x) = tell "throw " >> tellExpr x
tellExpr (EPre1 pre) = tellPre tellExpr pre
tellExpr (EPreN pre) = tellPre (sequence_ . intersperse (tellLn ";") . map tellExpr) pre
tellExpr (EUntyped e) = do { tell "(untyped "; tellExpr e; tell ")" }
tellExpr (EAnon xs) = do
    tell "({"
    sequence_ $ intersperse (tell ", ") $ map f xs
    tell "})"
    where f (n,e) = tell (n ++ ":") >> tellExpr e
tellExpr (ECast x Nothing) = do { tell "(cast "; tellExpr x; tell ")" }
tellExpr (ECast x (Just y))= do { tell "cast("; tellExpr x; tell ", "; tellType y; tell ")" }
tellExpr (EFunction f) = do { tell "(function "; tellFExpr f; tell ")" }
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
tellExpr (EFor x y) = do { tell "for("; tellExpr x; tell ")"; tellExpr y }
tellExpr (ETry x cs) = do
    tell "try"; tellExpr x; nl;
    mapM_ f cs
    where f (n,t,e) = do { tell $ "catch("++n++":"
                         ; tellType t
                         ; tell ")"
                         ; tellExpr e
                         ; nl }
tellExpr (ESwitch e cs y) = do
    tell "switch("; tellExpr e; tell ")"; openScope;
    mapM_ f cs
    maybeTell g y
    closeScope;
    where f (e,es) = do { tell "case "; tellExpr e; tell ":"
                        ; modify (second (+1))
                        ; mapM_ ((>> tell ";") . tellExpr) es; nl
                        ; modify (second (subtract 1)) }
          g es = do { tell "default:"
                    ; modify (second (+1))
                    ; mapM_ ((>> tell ";") . tellExpr) es; nl
                    ; modify (second (subtract 1)) }
tellExpr (EIf x y z) = do
    tell "if("; tellExpr x; tell ") "; tellExpr y; tell " "
    maybeTell ((tell "else " >>) . tellExpr) z

showUnop op = case op of
    OpInc -> "++"
    OpDec -> "--"
    OpNot -> "!"
    OpNeg -> "-"
    OpNegBits -> "~"

showOp op = case op of
    OpAdd -> "+"
    OpMul -> "*"
    OpDiv -> "/"
    OpSub -> "-"
    OpMod -> "%"
    OpAssign -> "="
    OpEq -> "=="
    OpNeq -> "!="
    OpGt -> ">"
    OpGeq -> ">="
    OpLt -> "<"
    OpLeq -> "<="
    OpOr -> "|"
    OpAnd -> "&"
    OpXor -> "^"
    OpBoolAnd -> "&&"
    OpBoolOr -> "||"
    OpShl -> "<<"
    OpShr -> ">>"
    OpUShr -> ">>>"
    OpInterval -> "..."
    OpAssignOp op -> showOp op ++ "="

-----------------------------------------------

tellType (BasicType x) = tell x
tellType (ParamType x xs)
    = do { tellType x; tell "<"; mapM_ tellType xs; tell ">" }
tellType (PreType pre) = tellPre tellType pre
tellType (FuncType x y)
    = do { tellType x; tell "->"; tellType y }

-----------------------------------------------

tellPre teller (cond, ife, elses, elsee) = do
    tell "#if "; tellCond cond; tell " "; teller ife;
    mapM_ tell_else elses
    maybeTell ((tell "#else " >>) . teller) elsee
    tell "#end "
    where
        tell_else (cond, ife)
            = do { tell "#elseif "; tellCond cond; teller ife }
        tellCond (PreIdent x) = tell x
        tellCond (PreAnd a b) = do { tell "("; tellCond a; tell "&&"; tellCond b; tell ")" }
        tellCond (PreOr a b) = do { tell "("; tellCond a; tell "||"; tellCond b; tell ")" }
        tellCond (PreNot a) = do { tell "(!"; tellCond a; tell ")" }

-----------------------------------------------

maybeTell = maybe (return ())
	
tellLn x = tell x >> nl

openScope = tellLn "{" >> modify (second (+1))
closeScope = maybeNl >> modify (second (subtract 1)) >> tellLn "}"

tell x = do
    (f,n) <- get
    when f $ replicateM_ n (W.tell "    ")
    W.tell x
    put (False,n)

maybeNl = liftM fst get >>= flip unless nl
nl = W.tell "\n" >> modify (first (const True))

