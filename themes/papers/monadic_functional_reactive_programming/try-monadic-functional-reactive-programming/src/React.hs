{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React (
	React, EvReqs, EvOccs, interpret, exper, first, done ) where

import Data.Set

import Freer
import FTCQueue
import TaggableFunction

type EvReqs = Set
type EvOccs = Set

data Rct e a where Await :: EvReqs e -> Rct e (EvOccs e)

type React s e a = Freer s FTCQueue (Taggable s) (Rct e) a

exper :: EvReqs e -> React s e (EvOccs e)
exper rs = Await rs >>>= pure

interpret :: Monad m => (EvReqs e -> m (EvOccs e)) -> React s e a -> m a
interpret _ (Pure x) = pure x
interpret p (Await e :>>= c) = p e >>= interpret p . qApp c

first :: Ord e => React s e a -> React s e b -> React s e (React s e a, React s e b)
first l r = case (l, r) of
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `union` er
		c b = first (update l b) (update r b) in
		Await e >>>= c
	_ -> Pure (l, r)

update :: Ord e => React s e a -> EvOccs e -> React s e a
update (Await e :>>= c) b | b' /= empty = c `qApp` b'
	where b' = b `filterOccs` e
update r _ = r

filterOccs :: Ord e => EvOccs e -> EvReqs e -> EvOccs e
filterOccs = intersection

done :: React s e a -> Maybe a
done (Pure x) = Just x; done _ = Nothing
