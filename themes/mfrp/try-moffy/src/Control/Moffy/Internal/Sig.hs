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
	-- * Copies
	spawn, parList ) where

import Prelude hiding (repeat, break, until)

import Control.Arrow (first, (>>>), (***))
import Control.Monad.Freer.Par (pattern Pure, pattern (:=<<))
import Control.Moffy.Internal.Sig.Type (
	Sig(..), ISig(..), isig,
	emit, emitAll, waitFor, repeat, res, ires, hold )
import Control.Moffy.Internal.React (
	Firstable, Adjustable, Updatable, adjust, par )
import Control.Moffy.Internal.React.Type (React, Rct(..))
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
	mf <*> mx = Flip $ unflip mf `app` unflip mx

instance (Mergeable es es es, Semigroup r) =>
	Applicative (Flip (ISig s es) r) where
	pure = Flip . (:| hold)
	mf <*> mx = Flip $ unflip mf `iapp` unflip mx

-- APP AND IAPP

app :: (Mergeable es es es, Semigroup r) =>
	Sig s es (a -> b) r -> Sig s es a r -> Sig s es b r
mf `app` mx = emitAll . uncurry (<*%>) =<< waitFor (mf `exposeBoth` mx)

exposeBoth :: (
	Updatable (ISig s es a r) (ISig s es b r'),
	Updatable (ISig s es b r') (ISig s es a r), Mergeable es es es ) =>
	Sig s es a r -> Sig s es b r' ->
	React s es (ISig s es a r, ISig s es b r')
l `exposeBoth` Sig r = do
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
-- PARALLEL
---------------------------------------------------------------------------

-- AT

infixr 7 `at`

at :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	React s (es :+: es') (Either r (Maybe a, r'))
(adjustSig -> Sig l) `at` (adjust -> r) = (l `par` r) >>= \case
	(Pure l', r') -> (first Just <$>) <$> l' `iat` r'
	(_, Pure y) -> pure $ Right (Nothing, y)
	(_ :=<< _, _ :=<< _) -> error "never occur"

iat :: (Updatable (ISig s es a r) r', Mergeable es es es) =>
	ISig s es a r -> React s es r' -> React s es (Either r (a, r'))
l `iat` r = (<$> ires (l `ipause` r)) \case
	(End x, _) -> Left x
	(h :| _, Pure y) -> Right (h, y)
	(_ :| _, _ :=<< _) -> error "never occur"

-- BREAK AND UNTIL

infixl 7 `break`, `until`

break :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
(adjustSig -> l) `break` (adjust -> r) = (<$> l `pause` r)
	$ first unSig >>> \case
		(Pure (End x), _) -> Left x
		(_ :=<< Await _, Pure r') -> Right (Nothing, r')
		(Pure (h :| _), Pure r') -> Right (Just h, r')
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

indexBy ::
	Firstable es es' (ISig s (es :+: es') a r) (ISig s (es :+: es') b r') =>
	Sig s es a r -> Sig s es' b r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
(adjustSig -> l) `indexBy` (adjustSig -> r) = l `indexBy_` r

indexBy_ :: (Updatable (ISig s es a r) (ISig s es b r'), Mergeable es es es) =>
	Sig s es a r -> Sig s es b r' -> Sig s es a (Either r (Maybe a, r'))
l `indexBy_` Sig r = waitFor (res $ l `pause` r) >>= \case
	(Sig (Pure l'), r') -> (first Just <$>) <$> l' `iindexBy` Sig r'
	(l', Pure (_ :| r')) -> l' `indexBy_` r'
	(_, Pure (End y)) -> pure $ Right (Nothing, y)
	_ -> error "never occur"

iindexBy :: (Updatable (ISig s es a r) (ISig s es b r'), Mergeable es es es) =>
	ISig s es a r -> Sig s es b r' -> Sig s es a (Either r (a, r'))
l `iindexBy` Sig r = waitFor (ires $ l `ipause` r) >>= \case
	(End x, _) -> pure $ Left x
	(l'@(hl :| _), Pure (_ :| r')) -> emit hl >> l' `iindexBy` r'
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
parList (Sig r) = iparList =<< waitFor r

iparList :: Mergeable es es es =>
	ISig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
iparList = isig (pure . ([] ,)) $ go . ((: []) <$>) . ((: []) <$%>) where
	go s (Sig r) = emitAll (s `ipause` r) >>= \case
		(s', Pure (h :| t)) -> go (h `cons` s') t
		(s', Pure (End y)) -> (, y) <$> emitAll s'
		(End x, r') -> emit [] >> ((x ++) `first`) <$> parList (Sig r')
		(_ :| _, _ :=<< _) -> error "never occur"

cons :: Mergeable es es es =>
	ISig s es a r -> ISig s es [a] [r] -> ISig s es [a] [r]
h `cons` t = uncurry (:) <$%> h `ipairs` t >>= \(h', t') ->
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

ipairs :: (Updatable (ISig s es a r) (ISig s es b r'), Mergeable es es es) =>
	ISig s es a r -> ISig s es b r' ->
	ISig s es (a, b) (ISig s es a r, ISig s es b r')
l@(End _) `ipairs` r = pure (l, r)
l `ipairs` r@(End _) = pure (l, r)
(hl :| Sig tl) `ipairs` (hr :| Sig tr) = ((hl, hr) :|) . Sig
	$ uncurry ipairs . ((hl ?:|) *** (hr ?:|)) <$> tl `par` tr
	where (?:|) h = \case Pure i -> i; t -> h :| Sig t

-- PAUSE

pause :: (Updatable (ISig s es a r) r', Mergeable es es es) =>
	Sig s es a r -> React s es r' ->
	Sig s es a (Sig s es a r, React s es r')
Sig l `pause` r = waitFor (l `par` r) >>= \case
	(Pure l', r') -> first emitAll <$> emitAll (l' `ipause` r')
	(l', r'@(Pure _)) -> pure (Sig l', r')
	_ -> error "never occur"

ipause :: (Updatable (ISig s es a r) r', Mergeable es es es) =>
	ISig s es a r -> React s es r' ->
	ISig s es a (ISig s es a r, React s es r')
l@(End _) `ipause` r = pure (l, r)
(h :| t) `ipause` r = (h :|) $ (<$> t `pause` r) \case
	(Sig (Pure t'), r') -> (t', r')
	(t', r'@(Pure _)) -> (h :| t', r')
	_ -> error "never occur"
