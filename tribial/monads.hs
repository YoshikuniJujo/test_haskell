{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Data.Monoid

instance Monoid e => Monad ((,) e) where
	return = (mempty ,)
	(e, x) >>= f = (e `mappend`) `first` f x

some :: (String, Int)
some = do
	x <- ("x is 3; ", 3)
	y <- ("y is 4; ", 4)
	("x + y; ", undefined)
	return $ x + y
