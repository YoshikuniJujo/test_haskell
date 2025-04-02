{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe where

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
	(_, Right (Yield o)) -> inj (Yield @_ @i o) `Bind` \x -> p =$= l x
	(Right Await, _) -> inj (Await @_ @o) `Bind` \i -> k i =$= p'
	(Right (Yield o), Right Await) -> k () =$= l (Just o)
	(Left u', Right Await) -> weaken u' `Bind` \x -> k x =$= p'
	(Right (Yield _), Left v') -> weaken v' `Bind` \x -> p =$= l x
	(Left u', Left _) -> weaken u' `Bind` \x -> k x =$= p'
p@(Pure r) =$= (v `Bind` l) = case decomp v of
	Right Await -> Pure r =$= l Nothing
	Right (Yield o) -> inj (Yield @_ @i o) `Bind` \x -> p =$= l x
	Left v' -> weaken v' `Bind` \x -> p =$= l x
