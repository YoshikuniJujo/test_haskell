{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Control.Moffy.Internal.Sig (
	-- * Repetition
	spawn, parList,
	-- * Parallel Composition
	at, break, until, indexBy ) where

import Prelude hiding (repeat, until, break)

import Control.Arrow ((***), first)
import Data.Type.Set ((:+:))
import Data.Type.Flip (Flip(..), (<$%>), (<*%>))
import Data.OneOrMore (Expandable, Mergeable)

import Control.Moffy.Internal.Sig.Type (
	Sig(..), ISig(..), isig,
	emit, emitAll, waitFor, res, ires, hold, repeat )
import Control.Moffy.Internal.React (Update, Adjustable, Firstable, adjust, par)
import Control.Moffy.Internal.React.Type (React, Rct(..), CollapsableOccurred)
import Control.Monad.Freer.Par (Freer(Pure), pattern (:>>=))

---------------------------------------------------------------------------

-- * FLIP APPLIICATIVE
-- * REPETITIVE COMBINATOR
--	+ SPAWN
--	+ PAR LIST
-- * PARALLEL COMBINATOR
--	+ AT
--	+ BREAK AND UNTIL
--	+ INDEX BY
-- * BASIC COMBINATOR
--	+ PAUSE
--	+ PAIRS
--	+ ADJUST

---------------------------------------------------------------------------
-- FLIP APPLICATIVE
---------------------------------------------------------------------------

instance (Mergeable es es es, Semigroup r) =>
	Applicative (Flip (Sig s es) r) where
	pure = Flip . Sig . pure . unflip . pure
	mf <*> mx = Flip $ unflip mf `app` unflip mx

instance (Mergeable es es es, Semigroup r) =>
	Applicative (Flip (ISig s es) r) where
	pure = Flip . (:| hold)
	mf <*> mx = Flip $ unflip mf `iapp` unflip mx

app :: (Mergeable es es es, Semigroup r) =>
	Sig s es (a -> b) r -> Sig s es a r -> Sig s es b r
mf `app` mx = emitAll . uncurry (<*%>) =<< waitFor (mf `bothStart` mx)

bothStart :: (
	Update (ISig s es a r) (ISig s es b r'),
	Update (ISig s es b r') (ISig s es a r),
	Mergeable es es es ) =>
	Sig s es a r -> Sig s es b r' ->
	React s es (ISig s es a r, ISig s es b r')
l `bothStart` Sig r = do
	(Sig l', r') <- res $ l `pause` r
	(Sig r'', l'') <- res $ Sig r' `pause` l'
	pure (ex l'', ex r'')
	where ex = \case Pure x -> x; _ -> error "never occur"

iapp :: (Mergeable es es es, Semigroup r) =>
	ISig s es (a -> b) r -> ISig s es a r -> ISig s es b r
mf `iapp` mx = (<$> (uncurry ($) <$%> mf `ipairs` mx)) \case
	(End x, End y) -> x <> y; (End x, _ :| _) -> x; (_ :| _, End y) -> y
	(_ :| _, _ :| _) -> error "never occur"

---------------------------------------------------------------------------
-- REPETITIVE COMBINATOR
---------------------------------------------------------------------------

-- SPAWN

spawn :: Sig s es a r -> Sig s es (ISig s es a r) ()
spawn = repeat . unSig

-- PAR 	LIST

parList :: Mergeable es es es =>
	Sig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
parList (Sig r) = iparList =<< waitFor r

iparList :: Mergeable es es es =>
	ISig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
iparList = isig (pure . ([] ,)) $ go . ((: []) <$>) . ((: []) <$%>) where
	go s (Sig r) = emitAll (s `ipause` r) >>= \case
		(s', Pure (h :| t)) -> go (h `cons` s') t
		(s', Pure (End y)) -> (, y) <$> emitAll s'
		(End x, r') -> emit [] >> ((x ++) `first`) <$> parList (Sig r')
		(_ :| _, _ :>>= _) -> error "never occur"

cons :: Mergeable es es es =>
	ISig s es a r -> ISig s es [a] [r] -> ISig s es [a] [r]
h `cons` t = uncurry (:) <$%> h `ipairs` t >>= \(h', t') ->
	(:) <$> ((: []) <$%> h') <*> t'

---------------------------------------------------------------------------
-- PARALLEL COMBINATOR
---------------------------------------------------------------------------

-- AT

infixr 7 `at`

at :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	React s (es :+: es') (Either r (a, r'))
(adjustSig -> l) `at` (adjust -> r) = res (l `pause` r) >>= \(Sig l', r') ->
	(<$> l') \case
		End x -> Left x
		h :| _ -> case r' of
			Pure y -> Right (h, y); _ :>>= _ -> error "never occur"

-- BREAK AND UNTIL

infixl 7 `break`, `until`

break :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
(adjustSig -> l) `break` (adjust -> r) = (<$> l `pause` r) \case
	(Sig (Pure (End x)), _) -> Left x
	(Sig (Await _ :>>= _), Pure r') -> Right (Nothing, r')
	(Sig (Pure (h :| _)), Pure r') -> Right (Just h, r')
	_ -> error "never occur"

until :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (a, r'))
(adjustSig -> l) `until` (adjust -> r) = l `pause` r >>= \(Sig l', r') ->
	(<$> waitFor l') \case
		End x -> Left x
		h :| _ -> case r' of
			Pure y -> Right (h, y); _ -> error "never occur"

-- INDEX BY

infixl 7 `indexBy`

indexBy :: (
	Update (ISig s (es :+: es') a r) (ISig s (es :+: es') b r'),
	Mergeable (es :+: es') (es :+: es') (es :+: es'),
	Adjustable es (es :+: es'), Adjustable es' (es :+: es')) =>
	Sig s es a r -> Sig s es' b r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
(adjustSig -> l) `indexBy` (adjustSig -> r) = l `indexBy_` r

indexBy_ :: (Update (ISig s es a r) (ISig s es b r'), Mergeable es es es ) =>
	Sig s es a r -> Sig s es b r' -> Sig s es a (Either r (Maybe a, r'))
l `indexBy_` Sig r = waitFor (res $ l `pause` r) >>= \case
	(Sig (Pure l'), r') -> (first Just <$>) <$> l' `iindexBy` Sig r'
	(Sig l', Pure (_ :| r')) -> Sig l' `indexBy_` r'
	(Sig _, Pure (End y)) -> pure $ Right (Nothing, y)
	_ -> error "never occur"

iindexBy :: (Update (ISig s es a r) (ISig s es b r'), Mergeable es es es) =>
	ISig s es a r -> Sig s es b r' -> Sig s es a (Either r (a, r'))
l `iindexBy` Sig r = waitFor (ires $ l `ipause` r) >>= \case
	(End x, _) -> pure $ Left x
	(l'@(hl :| _), Pure (_ :| r')) -> emit hl >> l' `iindexBy` r'
	(hl :| _, Pure (End y)) -> pure $ Right (hl, y)
	_ -> error "never occur"

---------------------------------------------------------------------------
-- BASIC COMBINATOR
---------------------------------------------------------------------------

-- PAUSE

pause :: (Update (ISig s es a r) r', Mergeable es es es) =>
	Sig s es a r -> React s es r' ->
	Sig s es a (Sig s es a r, React s es r')
Sig l `pause` r = waitFor (l `par` r) >>= \case
	(Pure l', r') -> first emitAll <$> emitAll (l' `ipause` r')
	(l', r'@(Pure _)) -> pure (Sig l', r')
	_ -> error "never occur"

ipause :: (Update (ISig s es a r) r', Mergeable es es es) =>
	ISig s es a r -> React s es r' ->
	ISig s es a (ISig s es a r, React s es r')
l@(End _) `ipause` r = pure (l, r)
(h :| t) `ipause` r = (h :|) $ (<$> (t `pause` r)) \case
	(Sig (Pure t'), r') -> (t', r')
	(t', r'@(Pure _)) -> (h :| t', r')
	_ -> error "never occur"

-- PAIRS

ipairs :: (
	Update (ISig s es a r) (ISig s es b r'),
	Mergeable es es es
	) => ISig s es a r -> ISig s es b r' -> ISig s es (a, b) (ISig s es a r, ISig s es b r')
l@(End _) `ipairs` r = pure (l, r)
l `ipairs` r@(End _) = pure (l, r)
(hl :| Sig tl) `ipairs` (hr :| Sig tr) = ((hl, hr) :|) . Sig
	$ uncurry ipairs . ((hl ?:|) *** (hr ?:|)) <$> tl `par` tr
	where (?:|) h = \case Pure t -> t; t -> h :| Sig t

-- ADJUST

adjustSig :: (
	CollapsableOccurred es' es,
	Expandable es es'
	) => Sig s es a r -> Sig s es' a r
adjustSig (Sig r) = Sig $ adjustISig <$> adjust r

adjustISig :: (
	CollapsableOccurred es' es,
	Expandable es es'
	) => ISig s es a r -> ISig s es' a r
adjustISig (End x) = End x
adjustISig (h :| t) = h :| adjustSig t
