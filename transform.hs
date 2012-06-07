module Transform (
        transform -- :: File -> File
    ) where

import Parser
import Control.Monad.State
import Control.Arrow (first,second)
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
funcTrans expr = let xs = unliftB expr in EBlock $ ftransform xs
    where
        ftransform exprs = let (xs,(lifts,_)) = runState (transx exprs) ([],0) in lifts ++ xs
        transx = liftM concat . mapM transf

-- get a new variable name from state
newvar = do
    (lifts,id) <- get
    let vname = "__newvar__" ++ show id
    put (EVars [(vname, Nothing, Nothing)] : lifts, id+1)
    return $ EConst (CIdent vname)

varname (EConst (CIdent vname)) = vname

-- unlift possible block of expressions to a list of expression
-- (inverse of liftB)
unliftB (EBlock xs) = xs
unliftB x = [x]

-- lift list of expressions to a block if not already.
liftB [EBlock xs] = EBlock xs
liftB xs = EBlock xs

-- apply transformation, possibly lifting to a block
transb = liftM liftB . transf

-- apply transformations to remove complex expressions from a statement
--   not allowed in c#
-- return list of statements
transf  :: Expr -> State ([Expr],Int) [Expr]

transf (EReturn (Just x)) = do
	(xs,x') <- uncomplicate x
	return $ xs ++ [EReturn (Just x')]
transf (EVars vs) = do
    ys' <- mapM (traverse uncomplicate . (\(_,_,z) -> z)) vs
    let ls = concatMap (maybe [] fst) ys'
    let vs' = zipWith (\(n,t,_) v -> (n,t,v)) vs (map (fmap snd) ys')
    return $ ls ++ [EVars vs']
transf (EBlock xs) = do
    xs' <- mapM transf xs
    return [EBlock (concat xs')]

-- seperate transf rule for if to keep structure where possible.
transf (EIf x y z) = do
    (xs,x') <- uncomplicate x
    ys' <- transb y
    zs' <- maybe (return Nothing) (liftM (Just . EBlock) . transf) z
    return $ xs ++ [EIf x' ys' zs']

transf (EFor x y) = do
    (xs,x') <- uncomplicate x
    y' <- transb y
    return $ xs ++ [EFor x' y']
transf (EWhile f x y) = do
    (xs,x') <- uncomplicate x
    y' <- transb y
    return $ xs ++ [EWhile f x' y']

-- seperate transf rule for switch to keep structure where possible.
transf (ESwitch x cs def) = do
    (xs,x') <- uncomplicate x
    let ms = map fst cs
    let es' = map (unliftB . funcTrans . liftB . snd) cs
    let def' = maybe Nothing (Just . unliftB . funcTrans . liftB) def
    return $ xs ++ [ESwitch x' (zip ms es') def']

-- default
transf x = do
    (xs,x') <- uncomplicate x
    return $ xs ++ [x']

-- uncomplicate an expression list
uncomplicateN :: [Expr] -> State ([Expr],Int) ([Expr],[Expr])
uncomplicateN xs = do
    xs' <- mapM uncomplicate xs
    return (concatMap fst xs', map snd xs')

-- uncomplicate single expression
-- return list of statements that need to be executed before expression is evaluated
--   and the new expression.
uncomplicate :: Expr -> State ([Expr],Int) ([Expr],Expr)

uncomplicate (ECall x y) = do
    (xs,x') <- uncomplicate x
    (ys,y') <- uncomplicateN y  
    return (xs ++ ys, ECall x' y')
uncomplicate (EArrayAccess x y) = do
    (xs,x') <- uncomplicate x
    (ys,y') <- uncomplicate y
    return (xs ++ ys, EArrayAccess x' y')
uncomplicate (EArray x) = do
    (xs,x') <- uncomplicateN x
    return (xs, EArray x')
uncomplicate (EThrow x) = do
    (xs,x') <- uncomplicate x
    return (xs, EThrow x')
uncomplicate (ENew t x) = do
    (xs,x') <- uncomplicateN x
    return (xs, ENew t x')
uncomplicate (EIf cond x (Just y)) = do
    (cs,c') <- uncomplicate cond
    (xs,x') <- uncomplicate x
    (ys,y') <- uncomplicate y
    return (cs++xs++ys, ETernary c' x' y')
uncomplicate (EBlock xs) = do
    retvar <- newvar -- out var for block to use
    fs <- liftM concat $ mapM transf xs
    let xs' = init fs
    (ys',y) <- uncomplicate (last fs) -- need to ensure this last statement is expression.
    return ([ EBlock $ xs' ++ ys' ++ [EBinop OpAssign retvar y] ], retvar)
uncomplicate (EBinop op x y) = do
    (xs,x') <- uncomplicate x
    (ys,y') <- uncomplicate y
    return (xs++ys, EBinop op x' y')
uncomplicate (ETernary x y z) = do
    (xs,x') <- uncomplicate x
    (ys,y') <- uncomplicate y
    (zs,z') <- uncomplicate z
    return (xs ++ ys ++ zs, ETernary x' y' z')
uncomplicate (EUnop op f x) = do
    (xs,x') <- uncomplicate x
    return (xs, EUnop op f x')
uncomplicate (EUntyped x) = do
    (xs,x') <- uncomplicate x
    return (xs, EUntyped x')
uncomplicate (ECast x t) = do
    (xs,x') <- uncomplicate x
    return (xs, ECast x' t)
uncomplicate (EAnon ns) = do
    (xs,x') <- (uncomplicateN . map snd) ns
    let ns' = zipWith (\(n,_) x -> (n,x)) ns x'
    return (xs, EAnon ns')
uncomplicate (EField x i) = do
    (xs,x') <- uncomplicate x
    return (xs, EField x' i)
uncomplicate (ESwitch x cs def) = do
    --transform into trivial switch
    (xs,x') <- uncomplicate x
    let ms = map fst cs
    (es,e') <- uncomplicateN (map (liftB . snd) cs)
    (ds,d') <- maybe (return ([],Nothing)) (liftM (second (Just . unliftB)) . uncomplicate . liftB) def

    --transform into block expression with if cascade
    tmpvar <- newvar -- tmpvar for if cascade
    let ifb = EBlock [
        EVars ([(varname tmpvar,Nothing,x')]),
        EIf (EBinop OpEq tmpvar (e'!!0)  .... etc
    
    return (xs ++ es ++ ds, ESwitch x' (zip ms (map unliftB e')) d')
-- default
uncomplicate x = return ([],x)

{-

    switch ( e ) {
        case a : b;
        default : c;
    }

    var retvar;
    {
        tmp = e;
        retvar = if(tmp==a) b
        else c
    }

    var retvar;
    {
        tmp = e;
        retvar = tmp==a ? b : c
    }

-}
