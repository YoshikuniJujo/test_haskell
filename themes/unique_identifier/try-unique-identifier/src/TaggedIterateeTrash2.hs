{-# LANGUAGE BlockArguments #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module TaggedIterateeTrash2 where

import FTCQueue

data It s i a = It { unIt :: Integer -> (DoneOrGet s i a, Integer) }
data DoneOrGet s i a = Done a | Get (TaggedExp s (DoneOrGet s i) i a)

instance Functor (It s i) where
	f `fmap` m = pure . f =<< m

instance Applicative (It s i) where
	pure x = It \c -> (Done x, c)
	(<*>) = (. flip (<$>)) . (>>=)

instance Monad (It s i) where
	It k >>= f = It \c -> case k c of
		(Done x, c') -> unIt (f x) c'
--		(Get k', c') -> let 
