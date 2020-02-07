{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryIdState where

data IdState a = IdState (Integer -> Integer) (Integer -> a)

instance Functor IdState where
	f `fmap` IdState u k = IdState ((+ 1) . u) $ f . k . u

instance Applicative IdState where
	pure x = IdState id $ const x
	mf <*> mx = (<$> mx) =<< mf

instance Monad IdState where
	IdState u k >>= f = IdState ((+ 1) . u) \s -> let IdState u' k' = f $ k s in k' $ u' s

get :: IdState Integer
get = IdState id id

sample1 :: IdState [Integer]
sample1 = do
	s1 <- get
	s2 <- get
	s3 <- get
	s4 <- get
	pure [s1, s2, s3, s4]

sample2 :: IdState [Integer]
sample2 =
	get >>= \s1 ->
	get >>= \s2 ->
	get >>= \s3 ->
	get >>= \s4 ->
	pure [s1, s2, s3, s4]

sample3 :: IdState [Integer]
sample3 =
	((get >>= \s1 -> get >>= \s2 -> pure (s1, s2)) >>= \(s1, s2) -> get >>= \s3 -> pure (s1, s2, s3)) >>= \(s1, s2, s3) -> get >>= \s4 -> pure [s1, s2, s3, s4]

runIdState :: IdState a -> Integer -> (a, Integer)
IdState u k `runIdState` s = (k s, u s)
