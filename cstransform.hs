module CSTransform (
        transform -- :: File -> File
    ) where

import Parser
import Transform
import Control.Monad.State
import Control.Arrow (first,second)
import Control.Applicative
import Data.Traversable (traverse)
import Data.Maybe (fromMaybe,mapMaybe)

transform (pckg,traits) = (pckg,traits')
    where
        traits' = map transt traits

        transt (FPre pre) = FPre $ transpre (map transt) pre
        transt (FClass t s ts) = FClass t s (map transc ts)
        transt x = x

        transc (CTrait as tinfo) = CTrait as (ttinfo tinfo)
        transc (CPre pre) = CPre $ transpre (map transc) pre

        ttinfo (Member var) = Member $ tvar var
        ttinfo (Method i fexpr) = Method i (tfexpr fexpr)
        ttinfo x = x

        tvar (i,t,e) = (i,t,liftM texpr e)
        tfexpr (ps,t,e) = (map (second tvar) ps, t, texpr e)

        texpr (EFor (EIn i (EBinop OpInterval bot top)) y)
            = EStdFor (EVars [(i,Nothing,Just bot)])
                      (EBinop OpLt itvar top)
                      (EUnop OpInc FlagPost itvar)
                      y
            where itvar = EConst (CIdent i)

        texpr (EFor (EIn i x) y) 
            = ECall forloop []
            where
                forloop = EFunction ([],Nothing,forblock)
                forblock = EBlock [EVars [("__iterator",Nothing, Just getite)], EWhile True (ECall (EField (EConst (CIdent "__iterator")) "hasNext") []) (EBlock [ EVars [(i,Nothing,Just $ ECall (EField (EConst (CIdent "__iterator")) "next") [])], y ]) ]
                getite = ECall (EField x "iterator") []

        texpr (EArray xs) = EArray $ map texpr xs
        texpr (EArrayAccess x y) = EArrayAccess (texpr x) (texpr y)
        texpr (EBlock xs) = EBlock $ map texpr xs
        texpr (EUnop op f e) = EUnop op f (texpr e)
        texpr (EBinop op x y) = EBinop op (texpr x) (texpr y)
        texpr (ETernary x y z) = ETernary (texpr x) (texpr y) (texpr z)
        texpr (EWhile f x y) = EWhile f (texpr x) (texpr y)
        texpr (EReturn x) = EReturn (liftM texpr x)
        texpr (EIf x y z) = EIf (texpr x) (texpr y) (liftM texpr z)
        texpr (EField x i) = EField (texpr x) i
        texpr (EThrow x) = EThrow (texpr x)
        texpr (ETry x cs) = ETry (texpr x) (map fc cs)
            where fc (i,t,e) = (i,t,texpr e)
        texpr (ENew t xs) = ENew t (map texpr xs)
        texpr (ESwitch x cs y) = ESwitch (texpr x) (map fc cs) (liftM (map texpr) y)
            where fc (x,xs) = (texpr x, map texpr xs)
        texpr (EVars vs) = EVars $ map tvar vs
        texpr (EFunction fexpr) = EFunction (tfexpr fexpr)
        texpr (ECast x t) = ECast (texpr x) t
        texpr (EPre1 pre) = EPre1 $ transpre texpr pre
        texpr (EPreN pre) = EPreN $ transpre (map texpr) pre
        texpr (EAnon vs) = EAnon $ map (second texpr) vs
        texpr (EUntyped x) = EUntyped $ texpr x
        texpr (EStdFor x y z w) = EStdFor (texpr x) (texpr y) (texpr z) (texpr w)

        texpr (ECall (EFunction f) xs)
            = ECall (EConst (CIdent "__call__")) (EFunction (tfexpr f) : map texpr xs)
        texpr (ECall x xs) = ECall (texpr x) (map texpr xs)

        texpr e = e
