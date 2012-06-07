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

uncomplicate (EBlock xs) = if (isExpr (EBlock xs)) then exprBlock else stdBlock
    where
        exprBlock = do
            f <- transf (xs!!0)
            uncomplicate (f!!0)
        stdBlock = do 
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
    let exprx = isPure x'
    tmpvar <- if exprx then (return x') else newvar
    let vare = if exprx then [] else [EVars [(varname tmpvar,Nothing,Just x')]]

    (fs,f') <- uncomplicate $ EBlock (vare ++ maybe [] return (cascade tmpvar (zip ms e') d'))

    return (xs ++ es ++ ds ++ fs, f')
    where
        cascade v []   Nothing = Nothing
        cascade v [] (Just xs) = Just (xs!!0)
        cascade v (i:is) elsee = Just (EIf (EBinop OpEq v (fst i)) (snd i) $ cascade v is elsee)

-- default
uncomplicate x = return ([],x)

-------------------------------------------

-- test if expression requires uncomplicating!
isExpr :: Expr -> Bool
isExpr (EConst c) = True
isExpr (EArray xs) = all isExpr xs
isExpr (EArrayAccess x y) = isExpr x && isExpr y
isExpr (EBlock xs) = length xs == 1 && isExpr (xs !! 0)
isExpr (EUnop _ _ x) = isExpr x
isExpr (EBinop _ x y) = isExpr x && isExpr y
isExpr (ETernary x y z) = all isExpr [x,y,z]
isExpr (EIn _ x) = isExpr x
isExpr (EIf x y z) = isExpr x && isExpr y && maybe True isExpr z
isExpr (EField x _) = isExpr x
isExpr (ECall x xs) = all isExpr (x:xs)
isExpr (ENew _ xs) = all isExpr xs
isExpr (ESwitch x cs def) = isExpr x && all (\(x,xs) -> isExpr x && (isExpr .EBlock) xs) cs && maybe True (isExpr . EBlock) def
isExpr (EFunction _) = True
isExpr (ECast x _) = isExpr x
isExpr (EAnon xs) = all (isExpr . snd) xs
isExpr (EUntyped x) = isExpr x

-- default
isExpr _ = False

-- test if true expression is also immutable and side-effect free
isPure :: Expr -> Bool
isPure x = isExpr x && pure x
    where 
        pure (EConst (CIdent _)) = False -- var may change
        pure (EConst _) = True
        pure (EArray xs) = all isPure xs
        pure (EArrayAccess x y) = isPure x && isPure y
        pure (EBlock xs) = length xs == 1 && isPure (xs !! 0)
        pure (EUnop OpInc _ _) = False
        pure (EUnop OpDec _ _) = False
        pure (EUnop _ _ x) = isPure x
        pure (EBinop OpAssign _ _) = False
        pure (EBinop (OpAssignOp _) _ _) = False
        pure (EBinop _ x y) = isPure x && isPure y
        pure (ETernary x y z) = all isPure [x,y,z]
        pure (EField x _) = isPure x
        pure (ECast x _) = isPure x
        -- don't allow pure EAnon to avoid duplication
        -- similarly for untyped/
        
        --default
        pure _ = False
