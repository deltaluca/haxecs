module Transform (
        transform -- :: File -> File
    ) where

import Parser
import Control.Monad.State
import Data.Traversable (traverse)

transform :: File -> File
transform (pckg,traits) = (pckg,traits')
    where
        traits' = map transf traits
        transf (FClass t s ctraits) = FClass t s (map ctransf ctraits)
        transf x = x

        ctransf (CTrait as tinfo) = CTrait as $ tinfof tinfo

        tinfof (Method name (ps,t,expr)) = Method name (ps,t,funcTrans expr)
        tinfof x = x

funcTrans :: Expr -> Expr
funcTrans expr = let xs = lift_e expr in EBlock $ ftransform xs
    where
        lift_e (EBlock xs) = xs
        lift_e x = [x]

        ftransform exprs = let (xs,(lifts,_)) = runState (transx exprs) ([],0) in lifts ++ xs
        transx = liftM concat . mapM transf

-- get a new variable name from state
newvar = do
    (lifts,id) <- get
    let vname = "__newvar__" ++ show id
    put (EVars [(vname, Nothing, Nothing)] : lifts, id+1)
    return $ EConst (CIdent vname)

-- apply transformations to remove complex expressions not allowed in c#
transf  :: Expr -> State ([Expr],Int) [Expr]
transf1 :: Expr -> State ([Expr],Int) Expr

transf (EReturn (Just x)) = do
	(ys,ret) <- uncomplicate x
	return $ ys ++ [EReturn (Just ret)]

transf (EVars vs) = do
    ys' <- mapM (traverse uncomplicate . (\(_,_,z) -> z)) vs
    let ls = concatMap (maybe [] fst) ys'
    let vs' = zipWith (\(n,t,_) v -> (n,t,v)) vs (map (fmap snd) ys')
    return $ ls ++ [EVars vs']

transf (EBinop op x y) = do
	(xs,x') <- uncomplicate x
	(ys,y') <- uncomplicate y
	return $ (xs++ys) ++ [EBinop op x' y']

-- defaults
transf x = liftM return . transf1 $ x
transf1 = return

-- uncomplicate an expression
uncomplicate :: Expr -> State ([Expr],Int) ([Expr],Expr)

-- default
uncomplicate (EIf cond x (Just y)) = do
	(cs,c') <- uncomplicate cond
	(xs,x') <- uncomplicate x
	(ys,y') <- uncomplicate y
	return (cs++xs++ys, ETernary c' x' y')

uncomplicate (EBlock xs) = do
	retvar <- newvar -- out var for block to use
	ys <- liftM concat $ mapM transf xs
	return ([ EBlock $ init ys ++ [EBinop OpAssign retvar (last ys)] ], retvar)

uncomplicate x = return ([],x)
