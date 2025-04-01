{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Pipe.Freer where

import Control.Monad

data Pipe i o r where
	Await :: Pipe i o (Maybe i)
	Yield :: o -> Pipe i o ()

data Freer t a
	= Pure a
	| forall x . Bind (t x) (x -> Freer t a)

instance Functor (Freer t) where
	fmap f = \case
		Pure x -> Pure $ f x
		Bind tx k -> Bind tx $ k >=> Pure . f

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> m = f <$> m
	Bind tx k <*> m = Bind tx $ k >=> (<$> m)

instance Monad (Freer t) where
	Pure x >>= f = f x
	Bind tx k >>= f = Bind tx $ k >=> f

freer :: t a -> Freer t a
freer = (`Bind` Pure)

await :: Freer (Pipe i o) (Maybe i)
await = freer Await

yield :: o -> Freer (Pipe i o) ()
yield = freer . Yield

runPipe :: Freer (Pipe i o) a -> Maybe a
runPipe m = case m of
	Pure x -> Just x
	_ -> Nothing

(=$=) :: Freer (Pipe i a) r -> Freer (Pipe a o) r' -> Freer (Pipe i o) r'
_ =$= Pure r = Pure r
p =$= (Yield o `Bind` p') = Yield o `Bind` \x -> p =$= p' x
(Await `Bind` p) =$= p' = Await `Bind` \i -> p i =$= p'
(Yield o `Bind` p) =$= (Await `Bind` p') = p () =$= p' (Just o)
Pure x =$= (Await `Bind` p') = Pure x =$= p' Nothing
