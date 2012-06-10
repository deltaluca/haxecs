module Transform (
         unliftB, liftB
        ,transpre, transpreM
    ) where

import Parser
import Control.Monad.State
import Control.Arrow (first,second)
import Control.Applicative
import Data.Traversable (traverse)

-- unlift possible block of expressions to a list of expression
-- (inverse of liftB)
unliftB (EBlock xs) = xs
unliftB x = [x]

-- lift list of expressions to a block if not already.
liftB [EBlock xs] = EBlock xs
liftB xs = EBlock xs

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
