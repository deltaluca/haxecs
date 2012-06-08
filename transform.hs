module Transform (
        transform -- :: File -> File
    ,transpre,transpreM,uncomplicate
    ) where

import Parser
import Control.Monad.State
import Control.Arrow (first,second)
import Control.Applicative
import Data.Traversable (traverse)

transform (pckg,traits) = (pckg,traits')
    where
        traits' = map transf traits
        transf (FClass t s ctraits) = FClass t s (map ctransf ctraits)
        transf (FPre pre) = FPre $ transpre (liftM transf) pre
        transf x = x

        ctransf (CTrait as tinfo) = CTrait as $ tinfof tinfo
        ctransf (CPre pre) = CPre $ transpre (liftM ctransf) pre

        tinfof (Method name (ps,t,expr)) = Method name (ps,t,funcTrans expr)
        tinfof x = x

funcTrans expr = let xs = unliftB expr in EBlock $ ftransform xs
    where
        ftransform exprs = fst $ runState (transx exprs) 0
        transx = liftM concat . mapM transf

-- get a new variable name from state
newvar = do
    id <- get
    let vname = "__t" ++ show id
    modify (+1)
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

-- transform Pre a
transpre :: (a -> b) -> Pre a -> Pre b
transpre xf (cond, ife, elses, elsee)
    = (cond, ife', elses', elsee')
    where
        ife' = xf ife
        elses' = map (second xf) elses
        elsee' = liftM xf elsee

transpreM :: (Applicative m, Monad m) => (a -> m b) -> Pre a -> m (Pre b)
transpreM xf (cond, ife, elses, elsee) = do
    ife' <- xf ife
    es' <- mapM (xf . snd) elses
    let elses' = zip (map fst elses) es'
    elsee' <- traverse xf elsee
    return (cond,ife',elses',elsee')

-- apply transformations to remove complex expressions from a statement
--   not allowed in c#
-- return list of statements
transf (EReturn (Just x)) = do
	x' <- uncomplicate x
	return [EReturn (Just x')]
transf (EVars vs) = do
    ys' <- mapM (traverse uncomplicate . (\(_,_,z) -> z)) vs
    let vs' = zipWith (\(n,t,_) v -> (n,t,v)) vs ys'
    return [EVars vs']


transf (EBlock [EBlock xs]) = transf (EBlock xs)
transf (EBlock xs) = do
    xs' <- mapM transf xs
    return [EBlock (concat xs')]

-- seperate transf rule for if to keep structure where possible.
transf (EIf x y z) = do
    x' <- uncomplicate x
    ys' <- transb y
    zs' <- maybe (return Nothing) (liftM (Just . EBlock) . transf) z
    return [EIf x' ys' zs']
transf (EFor x y) = do
    x' <- uncomplicate x
    y' <- transb y
    return [EFor x' y']
transf (EWhile f x y) = do
    x' <- uncomplicate x
    y' <- transb y
    return [EWhile f x' y']

-- seperate transf rule for switch to keep structure where possible.
transf (ESwitch x cs def) = do
    x' <- uncomplicate x
    let ms = map fst cs
    let es' = map (unliftB . funcTrans . liftB . snd) cs
    let def' = liftM (unliftB . funcTrans . liftB) def
    return [ESwitch x' (zip ms es') def']

transf (EPreN pre) = do
    pre' <- liftM EPreN (transpreM (liftM concat . mapM transf) pre)
    return [pre']

-- default
transf x = do
    x' <- uncomplicate x
    return [x']

-- uncomplicate an expression list
uncomplicateN = mapM uncomplicate

-- uncomplicate single expression
-- return list of statements that need to be executed before expression is evaluated
uncomplicate (ECall x y) = do
    x' <- uncomplicate x
    y' <- uncomplicateN y
    return $ ECall x' y'
uncomplicate (EArrayAccess x y) = do
    x' <- uncomplicate x
    y' <- uncomplicate y
    return $ EArrayAccess x' y'
uncomplicate (EArray x) = do
    x' <- uncomplicateN x
    return $ EArray x'
uncomplicate (EThrow x) = do
    x' <- uncomplicate x
    return $ EThrow x'
uncomplicate (ENew t x) = do
    x' <- uncomplicateN x
    return $ ENew t x'
uncomplicate (EIf cond x (Just y)) = do
    c' <- uncomplicate cond
    x' <- uncomplicate x
    y' <- uncomplicate y
    return $ ETernary c' x' y'

uncomplicate (EBlock [EBlock xs]) = uncomplicate (EBlock xs)
uncomplicate (EBlock xs) = if isExpr (EBlock xs) then exprBlock else stdBlock
    where
        exprBlock = transf (head xs) >>= uncomplicate . head
        stdBlock = do
            fs <- liftM concat $ mapM transf xs
            let xs' = init fs
            y <- uncomplicate (last fs) -- need to ensure this last statement is expression.
            return $ ECall (EFunction ([],Nothing,EBlock $ xs' ++ [EReturn $ Just y])) []

uncomplicate (EBinop op x y) = do
    x' <- uncomplicate x
    y' <- uncomplicate y
    return $ EBinop op x' y'
uncomplicate (ETernary x y z) = do
    x' <- uncomplicate x
    y' <- uncomplicate y
    z' <- uncomplicate z
    return $ ETernary x' y' z'
uncomplicate (EUnop op f x) = do
    x' <- uncomplicate x
    return $ EUnop op f x'
uncomplicate (EUntyped x) = do
    x' <- uncomplicate x
    return $ EUntyped x'
uncomplicate (ECast x t) = do
    x' <- uncomplicate x
    return $ ECast x' t
uncomplicate (EAnon ns) = do
    x' <- (uncomplicateN . map snd) ns
    let ns' = zipWith (\(n,_) x -> (n,x)) ns x'
    return $ EAnon ns'
uncomplicate (EField x i) = do
    x' <- uncomplicate x
    return $ EField x' i

uncomplicate (ESwitch x cs def) = do
    x' <- uncomplicate x
    let ms = map fst cs
    es' <- uncomplicateN (map (liftB . snd) cs)
    def' <- maybe (return Nothing) (liftM (Just . unliftB) . uncomplicate . liftB) def

    tmpvar <- newvar
    ife <- traverse uncomplicate (cascade tmpvar (zip ms es') def')

    return $ ECall (EFunction ([(False, (varname tmpvar, Nothing, Nothing))], Nothing, EReturn ife)) [x']
    where
        cascade v [] Nothing   = Nothing
        cascade v [] (Just xs) = Just (head xs)
        cascade v (i:is) elsee = Just (EIf (EBinop OpEq v (fst i)) (snd i) $ cascade v is elsee)

uncomplicate (EPre1 pre) = liftM EPre1 (transpreM uncomplicate pre)

-- default
uncomplicate x = return x

-------------------------------------------

-- test if expression requires uncomplicating!
isExpr (EConst c) = True
isExpr (EArray xs) = all isExpr xs
isExpr (EArrayAccess x y) = isExpr x && isExpr y
isExpr (EBlock xs) = length xs == 1 && isExpr (head xs)
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
