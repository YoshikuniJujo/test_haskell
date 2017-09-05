-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification, DeriveFunctor #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Typeable

import Free
import Cast

data Union a = forall t . (Functor t, Typeable t) => U (t a)

instance Functor Union where
	fmap f (U ta) = U $ fmap f ta

toUnion :: (Functor t, Typeable t) => t a -> Union a
toUnion = U

fromUnion :: (Functor t, Typeable t) => Union a -> Maybe (t a)
fromUnion (U ta) = cast1 ta

newtype Reader r w = Reader (r -> w) deriving Functor

ask :: Typeable r => Free Union r
ask = Free . U $ Reader Pure

run :: Free Union a -> a
run (Pure x) = x
run _ = error "Oh! No!"

runReader :: Typeable r => Free Union a -> r -> Free Union a
runReader f r = case f of
	Pure x -> Pure x
	Free u -> case fromUnion u of
		Just (Reader f') -> runReader (f' r) r
		Nothing -> Free $ fmap (`runReader` r) u

data State s w = State (s -> s) (s -> w) deriving Functor

getModify :: Typeable s => (s -> s) -> Free Union s
getModify f = Free . U $ State f Pure

get :: Typeable s => Free Union s
get = getModify id

modify :: Typeable s => (s -> s) -> Free Union ()
modify = (>> return ()) . getModify

put :: Typeable s => s -> Free Union ()
put = modify . const

runState :: Typeable s => Free Union a -> s -> Free Union (a, s)
runState f s = case f of
	Pure x -> Pure (x, s)
	Free u -> case fromUnion u of
		Just (State m f') -> runState (f' s) (m s)
		Nothing -> Free $ fmap (`runState` s) u

testRS :: Free Union (Char, Integer)
testRS = do
	r <- ask
	modify (+ (15 :: Integer))
	s <- get
	modify (+ (123 :: Integer))
	return (r, s)
