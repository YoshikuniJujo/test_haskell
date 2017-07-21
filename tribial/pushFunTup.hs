import Control.Comonad

--
-- fun :: Functor f => f (Maybe a, b) -> (Maybe (f a), f b)
-- fun 

{-
fun' :: Monad m => m (Maybe a, b) -> (Maybe (m a), maybe m b)
fun' m = do
	(x, y) <- m
	-}

{-
fun' :: Monad m => m (a, b) -> (m a, m b)
fun' m = do
-}

{-
some :: Comonad cm => cm (a, b) -> (cm a, cm b)
some cm = cm =>> extract cm
-}

{-
some :: f (a, b) -> (f a, f b)
-}

some :: Traversable f => f (Maybe a) -> Maybe (f a)
some = sequenceA

fun :: Traversable f => f (Maybe a, b) -> (Maybe (f a), f b)
fun v = (sequenceA $ fmap fst v, fmap snd v)
