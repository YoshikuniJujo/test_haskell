{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe where

import Control.Monad
import Data.Char
import System.IO
import Eff

data Pipe i o r where
	Await :: Pipe i o (Maybe i)
	Yield :: o -> Pipe i o ()

await :: forall i o effs . Member (Pipe i o) effs => Eff effs (Maybe i)
await = inj (Await @_ @o) `Bind` Pure

yield :: forall i o effs . Member (Pipe i o) effs => o -> Eff effs ()
yield = (`Bind` Pure) . inj . (Yield @_ @i)

runPipe :: Eff (Pipe i o ': effs) a -> Eff effs (Maybe a)
runPipe m = case m of
	Pure x -> Pure $ Just x
	u `Bind` k -> case decomp u of
		Right _ -> Pure Nothing
		Left u' -> u' `Bind` (runPipe . k)

(=$=) :: forall i a o effs r r' . Eff (Pipe i a ': effs) r -> Eff (Pipe a o ': effs) r' ->
	Eff (Pipe i o ': effs) r'
_ =$= Pure r = Pure r
p@(u `Bind` k) =$= p'@(v `Bind` l) = case (decomp u, decomp v) of
	(_, Left v') -> weaken v' `Bind` \x -> p =$= l x
	(_, Right (Yield o)) -> inj (Yield @_ @i o) `Bind` \x -> p =$= l x
	(Right Await, _) -> inj (Await @_ @o) `Bind` \i -> k i =$= p'
	(Right (Yield o), Right Await) -> k () =$= l (Just o)
	(Left u', Right Await) -> weaken u' `Bind` \x -> k x =$= p'
p@(Pure _) =$= (v `Bind` l) = case decomp v of
	Left v' -> weaken v' `Bind` \x -> p =$= l x
	Right Await -> p =$= l Nothing
	Right (Yield o) -> inj (Yield @_ @i o) `Bind` \x -> p =$= l x

print' :: (Member IO effs, Show a) => a -> Eff effs ()
print' = (`Bind` Pure) . inj . print

foo :: Eff (Pipe () Integer ': effs) ()
foo = yield @() 123

bar :: Member IO effs => Eff (Pipe Integer () ': effs) ()
bar = print' =<< await @Integer @()

lift :: Member m effs => m a -> Eff effs a
lift = (`Bind` Pure) . inj

hRead :: Member IO (Pipe () String ': effs) => Handle -> Eff (Pipe () String ': effs) ()
hRead h = do
	eof <- lift $ hIsEOF h
	if eof then return () else do
		l <- (`Bind` Pure) . inj $ hGetLine h
		yield @() l
		hRead h

writeString :: Member IO (Pipe String () ': effs) => Eff (Pipe String () ': effs) ()
writeString = do
	ms <- await @_ @()
	case ms of
		Just s -> lift (putStrLn s) >> writeString
		_ -> pure ()

takeP :: forall a effs . Int -> Eff (Pipe a a ': effs) ()
takeP 0 = pure ()
takeP n = do
	mx <- await @a @a
	case mx of
		Nothing -> pure ()
		Just x -> yield @a @a x >> takeP (n - 1)

convert :: forall a b effs . (a -> b) -> Eff (Pipe a b ': effs) ()
convert f = await @a @b >>= maybe (pure ()) ((>> convert f) . yield @a @b . f)

baz :: IO ()
baz = void . runM . runPipe
	$ hRead stdin =$= takeP 3 =$= convert (map toUpper) =$= writeString
