{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp.MyInterface (
	-- * Types
	Sig, ISig, React, Request(..), EvReqs, EvOccs, Mergeable, Or(..), Nihil, (:+:),
	-- * Run
	interpret, interpretSig,
	-- * React
	await', adjust, first',
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat', spawn, parList,
	-- * Parallel composition
	at, until', indexBy,
	-- * Flip Applicative
	(<$%>), fpure, (<*%>),
	-- * UnionList
	(>+.), singleton, expand, mergeMaybes, prj,
	-- * Sorted
	Sorted(Nil), (:-), Singleton, numbered
	) where

import Prelude hiding (map, repeat, scanl, until)

import MonadicFrp.Sig.Internal
import MonadicFrp.React
import Type.Flip
import Data.UnionList
import Data.Sorted hiding (Merge)

instance Functor (Flip (Sig es) r) where
	fmap f = Flip . map f . unflip

instance (
	Nihil es, Mergeable 'Nil es,
	(es :+: es) ~ es, Merge es es es,
	Semigroup r ) => Applicative (Flip (Sig es) r) where
	pure = Flip . always
	mf <*> mx = Flip $ unflip mf `app` unflip mx

app :: (
	(es :+: es) ~ es,
	Merge es es es,
	Collapse 'True (Occurred :$: es) (Occurred :$: es),
	Semigroup r ) => Sig es (a -> b) r -> Sig es a r -> Sig es b r
mf `app` mx = do
	(l, r) <- mf <^> mx
	case (l, r) of
		(End x, End y) -> pure $ x <> y
		(End x, _ :| _) -> pure x
		(_ :| _, End y) -> pure y
		(_ :| _, _ :| _) -> error "never occur"

await' :: a -> (Occurred a -> b) -> React (Singleton a) b
await' r f = await (singleton r) (pure . f . extract)
