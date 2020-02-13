{-# LANGUAGE BlockArguments, TupleSections, RankNTypes, GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreerWithTag (
	Freer(..), (>>>=), qApp, Tuple(..), qOpen, qClose,
	Count, runCount, addTag, openTag, closeTag ) where

import Control.Arrow

import FTCQueue

data Freer s t a = Pure a | forall x . t x :>>= EitherTagExp s (Freer s t) x a

instance Functor (Freer s t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (m :>>= k) = m :>>= (k |> Fun (Pure . f))

instance Applicative (Freer s t) where
	pure = Pure
	Pure f <*> mx = f <$> mx
	(m :>>= k) <*> mx = m :>>= (k |> Fun (<$> mx))

instance Monad (Freer s t) where
	Pure x >>= f = f x
	(m :>>= k) >>= f = m :>>= (k |> Fun f)

(>>>=) :: t a -> (a -> Freer s t a) -> Freer s t a
m >>>= f = m :>>= tsingleton (Fun f)

newtype Count s a = Count { unCount :: Integer -> (a, Integer) }

instance Functor (Count s) where
	f `fmap` Count k = Count $ (f `first`) . k

instance Applicative (Count s) where
	pure = Count . (,)
	Count k <*> mx = Count $ uncurry unCount . ((<$> mx) `first`) . k

instance Monad (Count a) where
	Count k >>= f = Count $ uncurry unCount . (f `first`) . k

countup :: Count s Integer
countup = Count $ id &&& (+ 1)

addTag :: Freer s t a -> Count s (Freer s t a)
addTag (m :>>= fs) = do
	t <- countup
	pure $ m :>>= (tsingleton (Open t) >< fs |> Close t)
addTag p = pure p

openTag, closeTag :: Freer s t a -> Maybe Integer
openTag (Pure _) = Nothing
openTag (_ :>>= fs) = case tviewl fs of
	TOne (Open t) -> Just t
	Open t :| _ -> Just t
	_ -> Nothing

closeTag (Pure _) = Nothing
closeTag (_ :>>= fs) = case tviewl fs of
	TOne (Close t) -> Just t
	Close t :| _ -> Just t
	_ -> Nothing

runCount :: (forall s . Count s a) -> a
runCount (Count k) = fst $ k 0

qApp :: EitherTagExp s (Freer s t) a b -> a -> Freer s t b
q `qApp` x = case tviewl q of
	TOne (Fun f) -> f x
	TOne (Open _) -> pure x
	TOne (Close _) -> pure x
	Fun f :| r -> case f x of
		Pure y -> r `qApp` y
		tx :>>= q' -> tx :>>= (q' >< r)
	Open _ :| r -> r `qApp` x
	Close _ :| r -> r `qApp` x

data Tuple s t b = forall x . Tuple (EitherTagExp s (Freer s t) x b) x

qOpen :: EitherTagExp s (Freer s t) a b -> a -> Either (Tuple s t b) (Freer s t b)
q `qOpen` x = case tviewl q of
	TOne (Fun f) -> Right $ f x
	TOne (Open _) -> Right $ pure x
	TOne (Close _) -> Right $ pure x
	Fun f :| r -> case f x of
		Pure y -> r `qOpen` y
		tx :>>= q' -> Right $ tx :>>= (q' >< r)
	Open _ :| _ -> Left $ Tuple q x
	Close _ :| r -> r `qOpen` x

qClose :: Integer -> EitherTagExp s (Freer s t) a b -> a -> Either (Tuple s t b) (Freer s t b)
qClose t q x = case tviewl q of
	TOne (Fun f) -> Right $ f x
	TOne (Open _) -> Right $ pure x
	TOne (Close _) -> Right $ pure x
	Fun f :| r -> case f x of
		Pure y -> qClose t r y
		tx :>>= q' -> Right $ tx :>>= (q' >< r)
	Open _ :| r -> qClose t r x
	Close t' :| r
		| t == t' -> Left $ Tuple q x
		| otherwise -> qClose t r x
