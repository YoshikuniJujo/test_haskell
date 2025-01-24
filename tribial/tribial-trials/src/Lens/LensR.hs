{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lens.LensR where

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type LensR s t a b = forall r . (a -> (r, Maybe b)) -> s -> (r, Maybe t)

type LensT s t a b = (s -> a, s -> b -> t)

type Foo s t a b = forall r . (a -> (r, ())) -> s -> (r, ())
type Bar s t a b = forall r . (a -> (r, b)) -> s -> (r, b)

lensFtoR :: Lens s t a b -> LensR s t a b
lensFtoR l f = runMyF . l (myF . f)

-- lensRtoF :: LensR s t a b -> Lens s t a b
-- lensRtoF lr f s = 

foo :: (forall r . (a -> (r, b)) -> s -> (r, t)) -> (a -> b) -> s -> t
foo l f s = snd $ l (\x -> ((), f x)) s

bar :: (forall r . (a -> (r, b)) -> (r, t)) -> (a -> b) -> t
bar l f = snd . l $ \x -> ((), f x)

-- bar' :: (forall r . (a -> (r, b)) -> (r, t)) -> (a -> f b) -> f t
-- bar' l f = snd . l $ \x -> ((), f x)

-- boo :: Functor f => ((a -> b) -> t) -> (a -> f b) -> f t
-- boo l f = l (\x -> f x)

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where
	fmap f = Identity . f . runIdentity

-- lensRtoT :: LensR s t a b -> LensT s t a b
-- lensRtoT l = (\s -> fst $ l (\x -> (x, Nothing)) s, \s b -> k

data MyF r a = MyF r (Maybe a) deriving Show

runMyF :: MyF r a -> (r, Maybe a)
runMyF (MyF r mx) = (r, mx)

myF :: (r, Maybe a) -> MyF r a
myF = uncurry MyF

instance Functor (MyF r) where
	f `fmap` MyF r mx = MyF r $ f <$> mx
