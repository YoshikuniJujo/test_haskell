{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafe.Pipe where

import Control.Monad
import Control.Monad.Yafe.Eff qualified as Eff
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union
import Data.FTCQueue qualified as FTCQueue
import Data.Char
import System.IO

data Pipe i o r where
	Await :: Pipe i o (Maybe i)
	Yield :: forall i o . o -> Pipe i o ()

await :: forall i o effs . Union.Member (Pipe i o) effs => Eff.E effs (Maybe i)
await = Eff.eff (Await @_ @o)

yield :: forall i o effs . Union.Member (Pipe i o) effs => o -> Eff.E effs ()
yield = Eff.eff . Yield @i

runPipe :: Eff.E (Pipe i o ': effs) a -> Eff.E effs (Maybe a)
runPipe = \case
	Freer.Pure x -> Freer.Pure $ Just x
	u `Freer.Bind` k -> case Union.decomp u of
		Left u' -> u' `Freer.Bind` FTCQueue.singleton (runPipe `Freer.comp` k)
		Right _ -> Freer.Pure Nothing

(=$=) :: forall i a o effs r r' .
	Eff.E (Pipe i a ': effs) r -> Eff.E (Pipe a o ': effs) r' ->
	Eff.E (Pipe i o ': effs) r'
_ =$= Freer.Pure r = Freer.Pure r
p@(u `Freer.Bind` k) =$= p'@(v `Freer.Bind` l) = case (Union.decomp u, Union.decomp v) of
	(_, Left v') ->
		Union.weaken v' `Freer.Bind`
		FTCQueue.singleton \x -> p =$= (l `Freer.app` x)
	(_, Right (Yield o)) ->
		Union.inj (Yield @i o) `Freer.Bind`
		FTCQueue.singleton \x -> p =$= (l `Freer.app` x)
	(Right Await, _) ->
		Union.inj (Await @_ @o) `Freer.Bind`
		FTCQueue.singleton \i -> (k `Freer.app` i) =$= p'
	(Right (Yield o), Right Await) ->
		(k `Freer.app` ()) =$= (l `Freer.app` Just o)
	(Left u', Right Await) ->
		Union.weaken u' `Freer.Bind`
		FTCQueue.singleton \x -> (k `Freer.app` x) =$= p'
p@(Freer.Pure _) =$= (v `Freer.Bind` l) = case Union.decomp v of
	Left v' -> Union.weaken v' `Freer.Bind`
		FTCQueue.singleton \x -> p =$= (l `Freer.app` x)
	Right Await -> p =$= (l `Freer.app` Nothing)
	Right (Yield o) -> Union.inj (Yield @i o) `Freer.Bind`
		FTCQueue.singleton \x -> p =$= (l `Freer.app` x)

print' :: (Union.Member IO effs, Show a) => a -> Eff.E effs ()
print' = Eff.eff . print

hRead :: Union.Member IO (Pipe () String ': effs) =>
	Handle -> Eff.E (Pipe () String ': effs) ()
hRead h = do
	eof <- Eff.eff $ hIsEOF h
	if eof then pure () else do
		l <- Eff.eff $ hGetLine h
		yield @() l
		hRead h

writeString :: Union.Member IO (Pipe String () ': effs) =>
	Eff.E (Pipe String () ': effs) ()
writeString = do
	ms <- await @_ @()
	case ms of
		Nothing -> pure ()
		Just s -> Eff.eff (putStrLn s) >> writeString

takeP :: forall a effs . Int -> Eff.E (Pipe a a ': effs) ()
takeP 0 = pure ()
takeP n = do
	mx <- await @a @a
	case mx of
		Nothing -> pure ()
		Just x -> yield @a @a x >> takeP (n - 1)

convert :: forall a b effs . (a -> b) -> Eff.E (Pipe a b ': effs) ()
convert f = await @a @b >>= maybe (pure ()) ((>> convert f) . yield @a @b . f)

baz :: IO ()
baz = void . Eff.runM . runPipe
	$ hRead stdin =$= takeP 3 =$= convert (map toUpper) =$= writeString
