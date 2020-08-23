{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Control.Moffy.Internal.Sig (
	-- * Adjust
	adjustSig,
	-- * Parallel
	at, break, until, indexBy,
	at_, break_, until_, indexBy_,
	-- * Copies
	spawn, parList, parList_,
	-- * Applicative Like
	app_, iapp_
	) where

import Prelude hiding (repeat, break, until)

import Control.Arrow (first, (>>>), (***))
import Control.Monad.Freer.Par (pattern Pure, pattern (:=<<))
import Control.Moffy.Internal.Sig.Type (
	Sig(..), ISig(..), isig,
	emit, emitAll, waitFor, repeat, res, ires, hold )
import Control.Moffy.Internal.React (
	Firstable, Adjustable, Updatable, adjust, par_ )
import Control.Moffy.Internal.React.Type (React, Rct(..), ThreadId, forkThreadId)
import Data.Type.Set ((:+:))
import Data.Type.Flip (Flip(..), (<$%>), (<*%>))
import Data.OneOrMore (Mergeable)

---------------------------------------------------------------------------

-- * FLIP APPLIICATIVE
--	+ INSTANCE
--	+ APP AND IAPP
-- * PARALLEL
--	+ AT
--	+ BREAK AND UNTIL
--	+ INDEX BY
-- * COPIES
--	+ SPAWN
--	+ PAR LIST
-- * BASIC COMBINATOR
--	+ ADJUST
--	+ PAIRS
--	+ PAUSE

---------------------------------------------------------------------------
-- FLIP APPLICATIVE
---------------------------------------------------------------------------

-- INSTANCE

instance (Mergeable es es es, Semigroup r) =>
	Applicative (Flip (Sig s es) r) where
	pure = Flip . Sig . pure . unflip . pure
	mf <*> mx = Flip $ app_ forkThreadId (unflip mf) (unflip mx)

instance (Mergeable es es es, Semigroup r) =>
	Applicative (Flip (ISig s es) r) where
	pure = Flip . (:| hold)
	mf <*> mx = Flip $ iapp_ forkThreadId (unflip mf) (unflip mx)

-- APP AND IAPP

app_ :: (Mergeable es es es, Semigroup r) =>
	React s es (ThreadId, ThreadId) -> Sig s es (a -> b) r -> Sig s es a r -> Sig s es b r
app_ ft mf mx = emitAll . uncurry (<*%>) =<< waitFor (exposeBoth_ ft mf mx)

exposeBoth_ :: (
	Updatable (ISig s es a r) (ISig s es b r'),
	Updatable (ISig s es b r') (ISig s es a r), Mergeable es es es ) =>
	React s es (ThreadId, ThreadId) ->
	Sig s es a r -> Sig s es b r' ->
	React s es (ISig s es a r, ISig s es b r')
exposeBoth_ ft l (Sig r) = do
	(Sig l', r') <- res $ pause_ ft l r
	(Sig r'', l'') <- res $ pause_ ft (Sig r') l'
	pure (ex l'', ex r'')
	where ex = \case Pure x -> x; _ -> error "never occur"

iapp_ :: (Mergeable es es es, Semigroup r) =>
	React s es (ThreadId, ThreadId) -> ISig s es (a -> b) r -> ISig s es a r -> ISig s es b r
iapp_ ft mf mx = (<$> (uncurry ($) <$%> ipairs_ ft mf mx)) \case
	(End x, End y) -> x <> y; (End x, _ :| _) -> x; (_ :| _, End y) -> y
	(_ :| _, _ :| _) -> error "never occur"

---------------------------------------------------------------------------
-- PARALLEL
---------------------------------------------------------------------------

-- AT

infixr 7 `at`

at :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	React s (es :+: es') (Either r (Maybe a, r'))
at = at_ forkThreadId

at_ :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	React s (es :+: es') (ThreadId, ThreadId) ->  Sig s es a r -> React s es' r' ->
	React s (es :+: es') (Either r (Maybe a, r'))
at_ ft (adjustSig -> Sig l) (adjust -> r) = (par_ ft l r) >>= \case
	(Pure l', r') -> (first Just <$>) <$> iat_ ft l' r'
	(_, Pure y) -> pure $ Right (Nothing, y)
	(_ :=<< _, _ :=<< _) -> error "never occur"

iat_ :: (Updatable (ISig s es a r) r', Mergeable es es es) =>
	React s es (ThreadId, ThreadId) -> ISig s es a r -> React s es r' -> React s es (Either r (a, r'))
iat_ ft l r = (<$> ires (ipause_ ft l r)) \case
	(End x, _) -> Left x
	(h :| _, Pure y) -> Right (h, y)
	(_ :| _, _ :=<< _) -> error "never occur"

-- BREAK AND UNTIL

infixl 7 `break`, `until`

break :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
break = break_ forkThreadId

until :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (a, r'))
until = until_ forkThreadId

break_ :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	React s (es :+: es') (ThreadId, ThreadId) -> Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
break_ ft (adjustSig -> l) (adjust -> r) = (<$> pause_ ft l r)
	$ first unSig >>> \case
		(Pure (End x), _) -> Left x
		(_ :=<< Await _, Pure r') -> Right (Nothing, r')
		(Pure (h :| _), Pure r') -> Right (Just h, r')
		_ -> error "never occur"

until_ :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	React s (es :+: es') (ThreadId, ThreadId) -> Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (a, r'))
until_ ft (adjustSig -> l) (adjust -> r) = pause_ ft l r >>= \(Sig l', r') ->
	(<$> waitFor l') \case
		End x -> Left x
		h :| _ -> case r' of
			Pure y -> Right (h, y); _ -> error "never occur"

-- INDEX BY

infixl 7 `indexBy`

indexBy ::
	Firstable es es' (ISig s (es :+: es') a r) (ISig s (es :+: es') b r') =>
	Sig s es a r -> Sig s es' b r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
indexBy = indexBy_ forkThreadId

indexBy_ ::
	Firstable es es' (ISig s (es :+: es') a r) (ISig s (es :+: es') b r') =>
	React s (es :+: es') (ThreadId, ThreadId) ->
	Sig s es a r -> Sig s es' b r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
indexBy_ ft (adjustSig -> l) (adjustSig -> r) = indexByGen_ ft l r

indexByGen_ :: (Updatable (ISig s es a r) (ISig s es b r'), Mergeable es es es) =>
	React s es (ThreadId, ThreadId) -> Sig s es a r -> Sig s es b r' -> Sig s es a (Either r (Maybe a, r'))
indexByGen_ ft l (Sig r) = waitFor (res $ pause_ ft l r) >>= \case
	(Sig (Pure l'), r') -> (first Just <$>) <$> iindexBy_ ft l' (Sig r')
	(l', Pure (_ :| r')) -> indexByGen_ ft l' r'
	(_, Pure (End y)) -> pure $ Right (Nothing, y)
	_ -> error "never occur"

iindexBy_ :: (Updatable (ISig s es a r) (ISig s es b r'), Mergeable es es es) =>
	React s es (ThreadId, ThreadId) -> ISig s es a r -> Sig s es b r' -> Sig s es a (Either r (a, r'))
iindexBy_ ft l (Sig r) = waitFor (ires $ ipause_ ft l r) >>= \case
	(End x, _) -> pure $ Left x
	(l'@(hl :| _), Pure (_ :| r')) -> emit hl >> iindexBy_ ft l' r'
	(hl :| _, Pure (End y)) -> pure $ Right (hl, y)
	_ -> error "never occur"

---------------------------------------------------------------------------
-- COPIES
---------------------------------------------------------------------------

-- SPAWN

spawn :: Sig s es a r -> Sig s es (ISig s es a r) r'
spawn = repeat . unSig

-- PAR 	LIST

parList :: Mergeable es es es =>
	Sig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
parList = parList_ forkThreadId

parList_ :: Mergeable es es es =>
	React s es (ThreadId, ThreadId) -> Sig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
parList_ ft (Sig r) = iparList_ ft =<< waitFor r

iparList_ :: Mergeable es es es =>
	React s es (ThreadId, ThreadId) -> ISig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
iparList_ ft = isig (pure . ([] ,)) $ go . ((: []) <$>) . ((: []) <$%>) where
	go s (Sig r) = emitAll (ipause_ ft s r) >>= \case
		(s', Pure (h :| t)) -> go (cons_ ft h s') t
		(s', Pure (End y)) -> (, y) <$> emitAll s'
		(End x, r') -> emit [] >> ((x ++) `first`) <$> parList_ ft (Sig r')
		(_ :| _, _ :=<< _) -> error "never occur"

cons_ :: Mergeable es es es =>
	React s es (ThreadId, ThreadId) -> ISig s es a r -> ISig s es [a] [r] -> ISig s es [a] [r]
cons_ ft h t = uncurry (:) <$%> ipairs_ ft h t >>= \(h', t') ->
	(:) <$> ((: []) <$%> h') <*> t'

---------------------------------------------------------------------------
-- BASIC COMBINATOR
---------------------------------------------------------------------------

-- ADJUST

adjustSig :: Adjustable es es' => Sig s es a r -> Sig s es' a r
adjustSig = Sig . (adjustISig <$>) . adjust . unSig

adjustISig :: Adjustable es es' => ISig s es a r -> ISig s es' a r
adjustISig = isig End $ (adjustSig >>>) . (:|)

-- PAIRS

ipairs_ :: (Updatable (ISig s es a r) (ISig s es b r'), Mergeable es es es) =>
	React s es (ThreadId, ThreadId) -> ISig s es a r -> ISig s es b r' ->
	ISig s es (a, b) (ISig s es a r, ISig s es b r')
ipairs_ _ l@(End _) r = pure (l, r)
ipairs_ _ l r@(End _) = pure (l, r)
ipairs_ ft (hl :| Sig tl) (hr :| Sig tr) = ((hl, hr) :|) . Sig
	$ uncurry (ipairs_ ft) . ((hl ?:|) *** (hr ?:|)) <$> par_ ft tl tr
	where (?:|) h = \case Pure i -> i; t -> h :| Sig t

-- PAUSE

pause_ :: (Updatable (ISig s es a r) r', Mergeable es es es) =>
	React s es (ThreadId, ThreadId) ->
	Sig s es a r -> React s es r' ->
	Sig s es a (Sig s es a r, React s es r')
pause_ ft (Sig l) r = waitFor (par_ ft l r) >>= \case
	(Pure l', r') -> first emitAll <$> emitAll (ipause_ ft l' r')
	(l', r'@(Pure _)) -> pure (Sig l', r')
	_ -> error "never occur"

ipause_ :: (Updatable (ISig s es a r) r', Mergeable es es es) =>
	React s es (ThreadId, ThreadId) -> ISig s es a r -> React s es r' ->
	ISig s es a (ISig s es a r, React s es r')
ipause_ _ l@(End _) r = pure (l, r)
ipause_ ft (h :| t) r = (h :|) $ (<$> pause_ ft t r) \case
	(Sig (Pure t'), r') -> (t', r')
	(t', r'@(Pure _)) -> (h :| t', r')
	_ -> error "never occur"
