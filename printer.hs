module Printer (
         showUnop, showOp
        ,tellPre,tellType
        ,maybeTell,tellLn,openScope,closeScope,tell,maybeNl,nl
    ) where

import Parser
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W (tell)
import Control.Monad.State
import Control.Arrow
import Data.List

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

tellPre :: (a -> StateT (Bool,Int) (Writer String) ()) -> Pre a -> StateT (Bool,Int) (Writer String) ()
tellPre teller (cond, ife, elses, elsee) = do
    tell "#if "; tellCond cond; tell " "; teller ife;
    mapM_ tellElse elses
    maybeTell ((tell "#else " >>) . teller) elsee
    tell "#end "
    where
        tellElse (cond, ife)
            = do { tell "#elseif "; tellCond cond; teller ife }
        tellCond (PreIdent x) = tell x
        tellCond (PreAnd a b) = do { tell "("; tellCond a; tell "&&"; tellCond b; tell ")" }
        tellCond (PreOr a b) = do { tell "("; tellCond a; tell "||"; tellCond b; tell ")" }
        tellCond (PreNot a) = do { tell "(!"; tellCond a; tell ")" }

-----------------------------------------------

tellType (BasicType x) = tell x
tellType (ParamType x xs)
    = do { tellType x; tell "<"; mapM_ tellType xs; tell ">" }
tellType (PreType pre) = tellPre tellType pre
tellType (FuncType x y)
    = do { tellType x; tell "->"; tellType y }

-----------------------------------------------

maybeTell = maybe (return ())

tellLn :: String -> StateT (Bool,Int) (Writer String) ()
tellLn x = tell x >> nl

openScope :: StateT (Bool,Int) (Writer String) ()
openScope = tellLn "{" >> modify (second (+1))

closeScope :: StateT (Bool,Int) (Writer String) ()
closeScope = maybeNl >> modify (second (subtract 1)) >> tellLn "}"

tell x = do
    (f,n) <- get
    when f $ replicateM_ n (W.tell "    ")
    W.tell x
    put (False,n)

maybeNl = liftM fst get >>= flip unless nl
nl = W.tell "\n" >> modify (first (const True))


