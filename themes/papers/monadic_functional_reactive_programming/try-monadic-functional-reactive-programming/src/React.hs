{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React (
	React, Update, EvReqs, EvOccs, interpret, exper, first, first', done, done', never) where

import Data.Maybe
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

first' :: Ord e => React s e a -> React s e b -> React s e (React s e a, React s e b)
first' l r = case (l, r) of
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `union` er
		c b = first' (update' l b) (update' r b) in
		Await e >>>= c
	_ -> Pure (l, r)

update' :: Ord e => React s e a -> EvOccs e -> React s e a
update' (Await e :>>= c) b | b' /= empty = c `qApp` b'
	where b' = b `filterOccs` e
update' r _ = r

first :: (Ord e, Update a b) => React s e a -> React s e b -> React s e (React s e a, React s e b)
first l r = case (l, r) of
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `union` er
		c b = let (u, u') = update l r b in first u u' in
		Await e >>>= c
	_ -> Pure (l, r)

class Update a b where
	update :: Ord e => React s e a -> React s e b -> EvOccs e -> (React s e a, React s e b)

instance {-# OVERLAPPABLE #-} Update a b where
--	update :: Ord e => React s e a -> React s e b -> EvOccs e -> (React s e a, React s e b)
	update r@(Await e :>>= c) r'@(Await e' :>>= c') b
		| b' /= empty, b'' /= empty = (c `qApp` b', c' `qApp` b'')
		| b' /= empty = (c `qApp` b', r')
		| b'' /= empty = (r, c' `qApp` b'')
		where
		b' = b `filterOccs` e
		b'' = b `filterOccs` e'
	update r r' _ = (r, r')

instance {-# OVERLAPPING #-} Update a a where
--	update :: Ord e => React s e a -> React s e a -> EvOccs e -> (React s e a, React s e a)
	update r@(Await e :>>= c) r'@(Await e' :>>= c') b
		| b' /= empty, b'' /= empty, b' == b'' = qAppPar c c' b' -- (c `qApp` b', c' `qApp` b'')
		| b' /= empty, b'' /= empty = (c `qApp` b', c' `qApp` b'')
		| b' /= empty = (c `qApp` b', r')
		| b'' /= empty = (r, c' `qApp` b'')
		where
		b' = b `filterOccs` e
		b'' = b `filterOccs` e'
	update r r' _ = (r, r')

filterOccs :: Ord e => EvOccs e -> EvReqs e -> EvOccs e
filterOccs = intersection

done :: React s e a -> Maybe a
done (Pure x) = Just x; done _ = Nothing

done' :: React s e a -> a
done' = fromJust . done

never :: React s e a
never = Await empty :>>= undefined
