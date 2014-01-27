{-# LANGUAGE TupleSections #-}

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Monad

import Control.Category
import Control.Arrow

newtype Parser s a = P { runP :: [s] -> Maybe (a, [s]) }
-- newtype Parser s a = P { runP :: [s] -> [(a, [s])] }

instance Monad (Parser s) where
	return x = P $ return . (x ,)
	P p >>= f = P $ (runP <$> f . fst <*> snd =<<) . p
	fail _ = mzero

instance MonadPlus (Parser s) where
	mzero = P $ const mzero
	P p1 `mplus` P p2 = P $ mplus <$> p1 <*> p2

{-
data StaticParser s = SP Bool [s]
newtype DynamicParser s a = DP ([s] -> (s, [s]))
data Parser s a = P (StaticParser s) (DynamicParser s a)
-}
