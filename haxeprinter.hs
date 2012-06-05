module HaxePrinter (
        printAST -- :: File -> String
    ) where

import Parser
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W (tell)
import Control.Monad.State
import Data.List

printAST file = snd $ runWriter (runStateT (writeAST file) (True,0)) 

writeAST (pckg,traits) = do
    tellLn $ "package " ++ pckg ++ ";"
    nl
    sequence_ $ map tellFTrait traits

tellFTrait (FImport pckg) = tellLn $ "import " ++ pckg ++ ";"
tellFTrait (FTypeDef x y) = do { tell "typedef "; tell_type x; tell "="; tell_type y; tellLn ";" }
tellFTrait (FPre pre) = (tell_pre (mapM_ tellFTrait) pre) >> nl
tellFTrait (FClass x s traits) = do 
    tell "class "; tell_type x
    maybeTell ((tell " extends " >>) . tell_type) s
    tell " "; openScope
    mapM_ tellCTrait traits
    closeScope

tellCTrait (CPre pre) = (tell_pre (mapM_ tellCTrait) pre)
tellCTrait (CTrait as tinfo) = do
    mapM_ ((>> tell " ") . tellMember) as
    tellTInfo tinfo
    where
        tellMember x = case(x) of
            APublic   -> tell "public"
            APrivate  -> tell "private"
            AStatic   -> tell "static"
            AOverride -> tell "override"
            AInline   -> tell "inline"
            APre pre  -> (tell_pre (mapM_ tellMember) pre)

tellTInfo (Member var) = do { tell "var "; tell_var var; tellLn ";" }
tellTInfo (Method n f) = do
    tell $ "function " ++ n
    tell_fexpr f
tellTInfo (Property n g s t) = do
    tell $ "var " ++ n ++ "(" ++ g ++ "," ++ s ++ ")" ++ ":"
    tell_type t
    tellLn ";"

tell_fexpr (ps,t,e) = do
    tell "("
    sequence_ $ intersperse (tell ", ") $ map tell_param ps
    tell ")"
    maybeTell ((tell ":" >>) . tell_type) t
    tell_expr e

tell_param (opt,var) = (when opt $ tell "?") >> tell_var var
tell_var (n,t,v) = do
    tell n
    maybeTell ((tell ":" >>) . tell_type) t
    maybeTell ((tell " = " >>) . tell_expr) v 

-----------------------------------------------

tell_expr (EConst (CInt x)) = tell $ show x
tell_expr (EConst (CFloat x)) = tell $ show x
tell_expr (EConst (CString x)) = tell x
tell_expr (EConst (CIdent x)) = tell x

tell_expr (EArray xs) = do
    tell "["
    sequence_ $ intersperse (tell ", ") $ map tell_expr xs
    tell "]"
tell_expr (EBlock xs) = do
    openScope
    mapM_ ((>> tell ";") . tell_expr) xs
    closeScope;
tell_expr (EVars vs) = do
    tell "var "
    sequence_ $ intersperse (tell ", ") $ map tell_var vs
tell_expr (EArrayAccess x y) = do { tell_expr x; tell "["; tell_expr y; tell "]" }
tell_expr (EUnop op flag e) = (tell "(") >> (case flag of
    FlagPre  -> (tell $ show_unop op) >> (tell_expr e)
    FlagPost -> (tell_expr e) >> (tell $ show_unop op) ) >> (tell ")")
tell_expr (EBinop op x y) = do { tell "("; tell_expr x; tell $ show_op op; tell_expr y; tell ")" }
tell_expr (ETernary x y z) = do { tell "("; tell_expr x; tell "?"; tell_expr y; tell ":"; tell_expr z; tell ")" }
tell_expr (EReturn x) = do
    tell "return "
    maybeTell tell_expr x
tell_expr (EIn x y) = do { tell "("; tell $ x ++ " in "; tell_expr y; tell ")" }
tell_expr (EContinue) = tell "continue"
tell_expr (EBreak) = tell "break"
tell_expr (EThrow x) = (tell "throw ") >> (tell_expr x)
tell_expr (EPre1 pre) = tell_pre tell_expr pre
tell_expr (EPreN pre) = tell_pre (sequence_ . intersperse (tellLn ";") . map tell_expr) pre
tell_expr (EUntyped e) = do { tell "(untyped "; tell_expr e; tell ")" }
tell_expr (EAnon xs) = do
    tell "({"
    sequence_ $ intersperse (tell ", ") $ map f xs
    tell "})"
    where f (n,e) = (tell $ n ++ ":") >> (tell_expr e)
tell_expr (ECast x Nothing) = do { tell "(cast "; tell_expr x; tell ")" }
tell_expr (ECast x (Just y))= do { tell "cast("; tell_expr x; tell ", "; tell_type y; tell ")" }
tell_expr (EFunction f) = do { tell "(function "; tell_fexpr f; tell ")" }
tell_expr (ENew t xs) = do
    tell "(new "; tell_type t; tell "("
    sequence_ $ intersperse (tell ", ") $ map tell_expr xs
    tell "))"
tell_expr (ECall x xs) = do
    tell "("; tell_expr x; tell "("
    sequence_ $ intersperse (tell ", ") $ map tell_expr xs
    tell "))"
tell_expr (EField x i) = (tell_expr x) >> (tell $ "." ++ i)
tell_expr (EWhile True x y) = do { tell "while("; tell_expr x; tell ")"; tell_expr y }
tell_expr (EWhile False x y) = do { tell "do "; tell_expr y; tell " while("; tell_expr x; tell ")" }
tell_expr (EFor x y) = do { tell "for("; tell_expr x; tell ")"; tell_expr y }
tell_expr (ETry x cs) = do
    tell "try"; tell_expr x; nl;
    mapM_ f cs
    where f (n,t,e) = do { tell $ "catch("++n++":"
                         ; tell_type t
                         ; tell ")"
                         ; tell_expr e
                         ; nl }
tell_expr (ESwitch e cs y) = do
    tell "switch("; tell_expr e; tell ")"; openScope;
    mapM_ f cs
    maybeTell g y
    closeScope;
    where f (e,es) = do { tell $ "case "; tell_expr e; tell ":"
                        ; mapM_ ((>> tell ";") . tell_expr) es }
          g es = do { tell $ "default:"
                    ; mapM_ ((>> tell ";") . tell_expr) es }

tell_expr e = tell "<expr>"

show_unop op = case op of
    OpInc -> "++"
    OpDec -> "--"
    OpNot -> "!"
    OpNeg -> "-"
    OpNegBits -> "~"

show_op op = case op of
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
    OpAssignOp op -> (show_op op) ++ "="

-----------------------------------------------

tell_type (BasicType x) = tell x
tell_type (ParamType x xs)
    = do { tell_type x; tell "<"; mapM_ tell_type xs; tell ">" }
tell_type (PreType pre) = tell_pre tell_type pre
tell_type (FuncType x y)
    = do { tell_type x; tell "->"; tell_type y }

-----------------------------------------------

tell_pre teller (cond, ife, elses, elsee) = do
    tell "#if "; tell_cond cond; tell " "; teller ife;
    mapM_ tell_else elses
    maybeTell ((tell "#else " >>) . teller) elsee
    tell "#end "
    where
        tell_else (cond, ife)
            = do { tell "#elseif "; tell_cond cond; teller ife }
        tell_cond (PreIdent x) = tell x

-----------------------------------------------

maybeTell f = maybe (return ()) f
	
tellLn x = tell x >> nl

openScope  = (tellLn "{") >> (modify (\(f,n) -> (f,n+1)))
closeScope = maybe_nl >> (modify (\(f,n) -> (f,n-1))) >> (tellLn "}")

tell x = do
    (f,n) <- get
    when f $ replicateM_ n (W.tell "    ")
    W.tell x
    put (False,n)

maybe_nl = do
    (f,_) <- get
    unless f $ nl

nl = do
    W.tell "\n"
    modify (\(_,n) -> (True,n))
