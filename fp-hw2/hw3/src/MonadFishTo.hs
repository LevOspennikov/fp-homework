import           MonadFish
import           MonadJoin

instance MonadFish m => Monad m where
    return = returnFish
    (>>=)  = flip (id >=>)

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join       = id >=> id
