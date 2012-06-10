module TypedefTransform (
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
        typedefs = concatMap gather traits
        ttype = flip resolve typedefs

        traits' = mapMaybe transt traits

        transt (FTypeDef _ _) = Nothing
        transt (FPre pre)
            = Just . FPre $ transpre (mapMaybe transt) pre
        transt (FImport i) = Just $ FImport i
        transt (FClass t s ts)
            = Just $ FClass (ttype t) (liftM ttype s) (map transc ts)

        transc (CTrait as tinfo) = CTrait as (ttinfo tinfo)
        transc (CPre pre) = CPre $ transpre (map transc) pre

        ttinfo (Member var) = Member $ tvar var
        ttinfo (Property a b c t) = Property a b c (ttype t)
        ttinfo (Method i fexpr) = Method i (tfexpr fexpr)

        tvar (i,t,e) = (i,liftM ttype t,liftM texpr e)
        tfexpr (ps,t,e) = (map (second tvar) ps, liftM ttype t, texpr e)

        texpr (EArray xs) = EArray $ map texpr xs
        texpr (EArrayAccess x y) = EArrayAccess (texpr x) (texpr y)
        texpr (EBlock xs) = EBlock $ map texpr xs
        texpr (EUnop op f e) = EUnop op f (texpr e)
        texpr (EBinop op x y) = EBinop op (texpr x) (texpr y)
        texpr (ETernary x y z) = ETernary (texpr x) (texpr y) (texpr z)
        texpr (EWhile f x y) = EWhile f (texpr x) (texpr y)
        texpr (EFor x y) = EFor (texpr x) (texpr y)
        texpr (EReturn x) = EReturn (liftM texpr x)
        texpr (EIf x y z) = EIf (texpr x) (texpr y) (liftM texpr z)
        texpr (EField x i) = EField (texpr x) i
        texpr (ECall x xs) = ECall (texpr x) (map texpr xs)
        texpr (EThrow x) = EThrow (texpr x)
        texpr (ETry x cs) = ETry (texpr x) (map fc cs)
            where fc (i,t,e) = (i,ttype t,texpr e)
        texpr (ENew t xs) = ENew (ttype t) (map texpr xs)
        texpr (ESwitch x cs y) = ESwitch (texpr x) (map fc cs) (liftM (map texpr) y)
            where fc (x,xs) = (texpr x, map texpr xs)
        texpr (EVars vs) = EVars $ map tvar vs
        texpr (EFunction fexpr) = EFunction (tfexpr fexpr)
        texpr (ECast x t) = ECast (texpr x) (liftM ttype t)
        texpr (EPre1 pre) = EPre1 $ transpre texpr pre
        texpr (EPreN pre) = EPreN $ transpre (map texpr) pre
        texpr (EAnon vs) = EAnon $ map (second texpr) vs
        texpr (EUntyped x) = EUntyped $ texpr x
        texpr (EUnchecked x) = EUnchecked $ texpr x
        texpr (EStdFor x y z w) = EStdFor (texpr x) (texpr y) (texpr z) (texpr w)
        texpr e = e

gather :: FileTrait -> [(Type,Type)]
gather (FTypeDef from to) = [(from,to)]
gather (FPre _) = error "Typedef must not be within a #if block"
gather x = []

resolve :: Type -> [(Type,Type)] -> Type
resolve (BasicType x) xs
    = maybe (BasicType x) (flip resolve xs) (lookup (BasicType x) xs)
resolve (FuncType x y) xs
    = maybe def (flip resolve xs) (lookup (FuncType x y) xs)
    where def = FuncType (resolve x xs) (resolve y xs)
resolve (ParamType t ts) xs
    = case lookup (ParamType t ts') xs of
        Just t' -> resolve t' xs
        Nothing -> case lookup t xs of
                        Just t' -> resolve (ParamType t' ts') xs
                        Nothing -> ParamType t ts'
    where ts' = map (flip resolve xs) ts
resolve (PreType pre) xs
    = PreType $ transpre (flip resolve xs) pre
