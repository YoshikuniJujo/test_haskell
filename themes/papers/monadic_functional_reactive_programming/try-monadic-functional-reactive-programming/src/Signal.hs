{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Signal (
	Sig, ISig, interpretSig,
	waitFor, emit, repeat, map, scanl, find,
	at, until, cur, always, (<^>), indexBy) where

import Control.Arrow ((***))
import Prelude hiding (repeat, map, scanl, break, until, tail)

import React
import Freer

newtype Sig s e a b = Sig { unSig :: React s e (ISig s e a b) }
data ISig s e a b = End b | a :| Sig s e a b

interpretSig :: Monad m =>
	(EvReqs e -> m (EvOccs e)) -> (a -> m r) -> Sig s e a b -> m b
interpretSig p d (Sig s) = interpret p s >>=
	\case h :| t -> d h >> interpretSig p d t; End a -> pure a

waitFor :: React s e b -> Sig s e a b
waitFor x = Sig $ End <$> x

repeat :: React s e a -> Sig s e a ()
repeat x = xs where xs = Sig $ (:| xs) <$> x

map :: (a -> b) -> Sig s e a r -> Sig s e b r
f `map` Sig l = Sig $ imap f `fmap` l

imap :: (a -> b) -> ISig s e a r -> ISig s e b r
_ `imap` End x = End x
f `imap` (h :| t) = f h :| (f `map` t)

scanl :: (a -> b -> a) -> a -> Sig s e b r -> Sig s e a r
scanl f i l = emitAll $ iscanl f i l

iscanl :: (a -> b -> a) -> a -> Sig s e b r -> ISig s e a r
iscanl f i (Sig l) = i :| (waitFor l >>= lsl)
	where
	lsl (h :| t) = scanl f (f i h) t
	lsl (End x) = pure x

find :: (a -> Bool) -> Sig s e a r -> React s e (Either a r)
find p l = icur <$> res (break p l)

break :: (a -> Bool) -> Sig s e a b -> Sig s e a (ISig s e a b)
break p (Sig l) = Sig $ ibreak p <$> l

ibreak :: (a -> Bool) -> ISig s e a b -> ISig s e a (ISig s e a b)
ibreak p (h :| t)
	| p h = pure $ h :| t
	| otherwise = h :| break p t
ibreak _ (End x) = pure $ End x

res :: Sig s e a b -> React s e b
res (Sig l) = l >>= ires

ires :: ISig s e a b -> React s e b
ires (_ :| t) = res t; ires (End x) = pure x

cur :: Sig s e a b ->  Maybe a
cur (Sig (Pure is)) = either Just (const Nothing) $ icur is
cur _ = Nothing

icur :: ISig s e a b -> Either a b
icur (h :| _) = Left h
icur (End x) = Right x

at :: (Ord e, Update (ISig s e a y) b) => Sig s e a y -> React s e b -> React s e (Maybe a)
l `at` a = cur . fst <$> res (l `until` a)

until :: (Ord e, Update (ISig s e a b) c) => Sig s e a b -> React s e c -> Sig s e a (Sig s e a b, React s e c)
until (Sig l) a = waitFor (first l a) >>= un where
	un (Pure l', a') = do
		(l'', a'') <- emitAll $ l' `iuntil` a'
		pure (emitAll l'', a'')
	un (l', a') = pure (Sig l', a')

iuntil :: (Ord e, Update (ISig s e a b) c) => ISig s e a b -> React s e c -> ISig s e a (ISig s e a b, React s e c)
End l `iuntil` a = End (End l, a)
(h :| Sig t) `iuntil` a = h :| Sig (cont <$> first t a)
	where
	cont (Pure l, a') = l `iuntil` a'
	cont (t', Pure a') = End (h :| Sig t', Pure a')
	cont _ = error "never occur"

always :: a -> Sig s e a ()
always x = emit x >> hold

hold :: Sig s e a b
hold = waitFor never

(<^>) :: Ord e => Sig s e (a -> b) l -> Sig s e a r -> Sig s e b (Sig s e (a -> b) l, Sig s e a r)
l <^> r = do
	(l', r') <- waitFor (bothStart l r)
	emitAll $ (\(f, a) -> f a) `imap` ((emitAll *** emitAll) <$> pairs l' r')

bothStart :: (Ord e, Update (ISig s e a r) (ISig s e b l), Update (ISig s e b l) (ISig s e a r)) =>
	Sig s e a r -> Sig s e b l -> React s e (ISig s e a r, ISig s e b l)
bothStart l (Sig r) = do
	(Sig l', r') <- res $ l `until` r
	(Sig r'', l'') <- res $ Sig r' `until` l'
	pure (done' l'', done' r'')

pairs :: (Ord e, Update (ISig s e a l) (ISig s e b r)) => ISig s e a l -> ISig s e b r -> ISig s e (a, b) (ISig s e a l, ISig s e b r)
pairs (End a) b = End (End a, b)
pairs a (End b) = End (a, End b)
pairs (hl :| Sig tl) (hr :| Sig tr) = (hl, hr) :| tail
	where
	tail = Sig $ cont <$> tl `first` tr
	cont (tl', tr') = pairs (lup hl tl') (lup hr tr')
	lup _ (Pure l) = l; lup h t = h :| Sig t

indexBy :: (Ord e, Update (ISig s e a l) (ISig s e b r)) => Sig s e a l -> Sig s e b r -> Sig s e a ()
l `indexBy` Sig r = do
	(Sig l', r') <- waitFor . res $ l `until` r
	case (l', r') of
		(_, Pure (End _)) -> pure ()
		(Pure l'', r'') -> l'' `iindexBy` Sig r''
		(l'', Pure (_ :| r'')) -> Sig l'' `indexBy` r''
		(_, _ :>>= _) -> error "never occur"

iindexBy :: (Ord e, Update (ISig s e a l) (ISig s e b r)) => ISig s e a l -> Sig s e b r -> Sig s e a ()
l `iindexBy` (Sig r) = waitFor ( ires $ l `iuntil` r) >>= \case
	(hl :| tl, Pure (_ :| tr)) -> emit hl >> (hl :| tl) `iindexBy` tr
	_ -> pure ()

instance Functor (Sig s e a) where
	f `fmap` Sig l = Sig $ (f <$>) <$> l

instance Applicative (Sig s e a) where
	pure = Sig . pure . pure
	Sig l <*> mx = Sig $ l >>= \case
		End f -> unSig $ f <$> mx
		h :| t -> pure $ h :| ((<$> mx) =<< t)

instance Monad (Sig s e a) where
	Sig l >>= f = Sig $ l >>=
		\case End x -> unSig $ f x; h :| t -> pure $ h :| (f =<< t)

emit :: a -> Sig s e a ()
emit x = emitAll $ x :| pure ()

emitAll :: ISig s e a b -> Sig s e a b
emitAll = Sig . pure

instance Functor (ISig s e a) where
	f `fmap` End x = End $ f x
	f `fmap` (h :| t) = h :| (f <$> t)

instance Applicative (ISig s e a) where
	pure = End
	End f <*> mx = f <$> mx
	(h :| t) <*> mx = h :| (t >>= emitAll . (<$> mx))

instance Monad (ISig s e a) where
	End x >>= f = f x
	(h :| t) >>= f = h :| (t >>= emitAll . f)
