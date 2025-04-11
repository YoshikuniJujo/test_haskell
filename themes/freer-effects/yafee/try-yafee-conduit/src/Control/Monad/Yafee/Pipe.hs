{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Pipe where

import Control.Arrow
import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union
import Data.FTCQueue qualified as FTCQueue
import Data.Char
import System.IO

data P i o r where Await :: P i o (Maybe i); Yield :: forall i o . o -> P i o ()

await :: forall i o effs . Eff.E (P i o ': effs) (Maybe i)
await = Eff.eff (Await @_ @o)

yield :: forall i o effs . o -> Eff.E (P i o ': effs) ()
yield = Eff.eff . Yield @i

await' :: forall i effs . forall o -> Union.Member (P i o) effs => Eff.E effs (Maybe i)
await' o = Eff.eff (Await @_ @o)

yield' :: forall o effs . forall i -> Union.Member (P i o) effs => o -> Eff.E effs ()
yield' i = Eff.eff . Yield @i

run :: Eff.E (P i o ': effs) a -> Eff.E effs (a, [o])
run = \case
	Freer.Pure x -> Freer.Pure $ (x, [])
	u Freer.:>>= k -> case Union.decomp u of
		Left u' -> u' Freer.:>>= FTCQueue.singleton (run `Freer.comp` k)
		Right Await -> run $ k `Freer.app` Nothing
		Right (Yield o) -> ((o :) `second`) <$> run (k `Freer.app` ())

(=$=) :: forall i a o effs r r' .
	Eff.E (P i a ': effs) r -> Eff.E (P a o ': effs) r' ->
	Eff.E (P i o ': effs) r'
_ =$= Freer.Pure r = Freer.Pure r
p@(u Freer.:>>= k) =$= p'@(v Freer.:>>= l) =
	case (Union.decomp u, Union.decomp v) of
		(_, Left v') ->
			Union.weaken v' Freer.:>>=
			FTCQueue.singleton ((p =$=) . (l `Freer.app`))
		(_, Right (Yield o)) ->
			Union.inj (Yield @i o) Freer.:>>=
			FTCQueue.singleton ((p =$=) . (l `Freer.app`))
		(Right Await, _) ->
			Union.inj (Await @_ @o) Freer.:>>=
			FTCQueue.singleton ((=$= p') . (k `Freer.app`))
		(Right (Yield o), Right Await) ->
			(k `Freer.app` ()) =$= (l `Freer.app` Just o)
		(Left u', Right Await) ->
			Union.weaken u' Freer.:>>=
			FTCQueue.singleton ((=$= p') . (k `Freer.app`))
p@(Freer.Pure _) =$= (v Freer.:>>= l) = case Union.decomp v of
	Left v' -> Union.weaken v' Freer.:>>=
		FTCQueue.singleton ((p =$=) . (l `Freer.app`))
	Right Await -> p =$= (l `Freer.app` Nothing)
	Right (Yield o) -> Union.inj (Yield @i o) Freer.:>>=
		FTCQueue.singleton ((p =$=) . (l `Freer.app`))

(=@=) :: forall i a o effs r r' .
	Eff.E (P i a ': effs) r -> Eff.E (P a o ': effs) r' ->
	Eff.E (P i o ': effs) (r, [a])
Freer.Pure r =@= _ = Freer.Pure (r, [])
p@(u Freer.:>>= k) =@= p'@(v Freer.:>>= l) =
	case (Union.decomp u, Union.decomp v) of
		(Left u', _) ->
			Union.weaken u' Freer.:>>=
			FTCQueue.singleton ((=@= p') . (k `Freer.app`))
		(_, Right (Yield o)) ->
			Union.inj (Yield @i o) Freer.:>>=
			FTCQueue.singleton ((p =@=) . (l `Freer.app`))
		(Right Await, _) ->
			Union.inj (Await @_ @o) Freer.:>>=
			FTCQueue.singleton ((=@= p') . (k `Freer.app`))
		(Right (Yield o), Right Await) ->
			(k `Freer.app` ()) =@= (l `Freer.app` Just o)
		(Right (Yield _o), Left v') ->
			Union.weaken v' Freer.:>>=
			FTCQueue.singleton ((p =@=) . (l `Freer.app`))
(u Freer.:>>= k) =@= p'@(Freer.Pure _) = case Union.decomp u of
	Left u' -> Union.weaken u' Freer.:>>=
		FTCQueue.singleton ((=@= p') . (k `Freer.app`))
	Right Await -> Union.inj (Await @i @o) Freer.:>>=
		FTCQueue.singleton ((=@= p') . (k `Freer.app`))
	Right (Yield o) -> ((o :) `second`) <$> ((k `Freer.app` ()) =@= p')

-- * TOOLS

convert :: forall a b effs . (a -> b) -> Eff.E (P a b ': effs) ()
convert f = await' b >>= maybe (pure ()) ((>> convert f) . yield' a . f)

-- * EXAMPLES

print' :: (Union.Member IO effs, Show a) => a -> Eff.E effs ()
print' = Eff.eff . print

hRead :: Union.Member IO (P () String ': effs) =>
	Handle -> Eff.E (P () String ': effs) ()
hRead h = do
	eof <- Eff.eff $ hIsEOF h
	if eof then pure () else do
		l <- Eff.eff $ hGetLine h
		yield' (type ()) l
		hRead h

writeString :: Union.Member IO (P String () ': effs) =>
	Eff.E (P String () ': effs) ()
writeString = do
	ms <- await' (type ())
	case ms of
		Nothing -> pure ()
		Just s -> Eff.eff (putStrLn s) >> writeString

takeP :: forall a effs . Int -> Eff.E (P a a ': effs) ()
takeP 0 = pure ()
takeP n = do
	mx <- await' a
	case mx of
		Nothing -> pure ()
		Just x -> yield' @a a x >> takeP (n - 1)

baz :: IO ()
baz = void . Eff.runM . run
	$ hRead stdin =$= takeP 3 =$= convert (map toUpper) =$= writeString
