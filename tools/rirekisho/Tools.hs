module Tools (unfoldrM) where

import Control.Monad

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f z = f z >>= step
	where
	step (Just (a, b')) = liftM (a :) $ unfoldrM f b'
	step Nothing = return []
