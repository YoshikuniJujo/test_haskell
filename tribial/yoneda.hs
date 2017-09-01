{-# LANGUAGE RankNTypes, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

newtype Yoneda f a = Yoneda (forall b . (a -> b) -> f b)
data Coyoneda f a = forall b . Coyoneda (b -> a) (f b)
data Coyoneda2 f b = forall a . Coyoneda2 (f a) (a -> b)
data Coyoneda3 f b = forall a . Coyoneda3 (a -> b) (f a)
data Coyoneda' f a where
	Coyoneda' :: (b -> a) -> f b -> Coyoneda' f a

exYoneda :: Yoneda IO String
exYoneda = Yoneda $ \f -> f <$> getLine

runYoneda :: Yoneda f a -> (a -> b) -> f b
runYoneda (Yoneda y) = y

exCoyoneda :: Coyoneda IO Int
exCoyoneda = Coyoneda read getLine

data Hoge a = Hoge a deriving Show

instance Functor (Yoneda f) where
	fmap f m = Yoneda (\k -> runYoneda m (k . f))

-- liftYoneda :: m a -> Yoneda m a
-- liftYoneda m = Yoneda $ \f -> f <$> m

instance Functor (Coyoneda f) where
	fmap f (Coyoneda g v) = Coyoneda (f . g) v

liftCoyoneda :: m a -> Coyoneda m a
liftCoyoneda m = Coyoneda id m
