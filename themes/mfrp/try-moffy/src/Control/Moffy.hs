{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy (
	-- * Sig
	Sig, ISig,
	-- * React
	React, Rct, EvReqs, EvOccs, Request(..),
	-- * Constraint
	Firstable, Adjustable,
	-- * Combinator
	-- ** Await and Adjust
	await, adjust, adjustSig, never,
	-- ** Create Sig
	emit, waitFor, repeat,
	-- ** Traverse
	find, scanl,
	-- ** Parallel
	first, at, break, until, indexBy,
	-- ** Copies
	spawn, parList ) where

import Prelude hiding (repeat, scanl, until, break)

import Control.Moffy.Internal.Sig (
	adjustSig, at_, break_, until_, indexBy_, spawn, parList_ )
import Control.Moffy.Internal.Sig.Type (
	Sig, ISig, emit, waitFor, repeat, find, scanl )
import Control.Moffy.Internal.React (Firstable, Adjustable, first_, adjust)
import Control.Moffy.Internal.React.Type (
	React, Rct, EvReqs, EvOccs, Request(..), await, forkThreadId, never )
import Data.Type.Set ((:+:))
import Data.OneOrMore (Mergeable)
import Data.Or (Or)

---------------------------------------------------------------------------

-- * PARALLEL
-- * COPIES

---------------------------------------------------------------------------
-- PARALLEL
---------------------------------------------------------------------------

infixr 8 `first`

first :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
first = first_ forkThreadId

infixr 7 `at`

at :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	React s (es :+: es') (Either r (Maybe a, r'))
at = at_ forkThreadId

infixl 7 `break`, `until`

break :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
break = break_ forkThreadId

until :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (a, r'))
until = until_ forkThreadId

infixl 7 `indexBy`

indexBy ::
	Firstable es es' (ISig s (es :+: es') a r) (ISig s (es :+: es') b r') =>
	Sig s es a r -> Sig s es' b r' ->
	Sig s (es :+: es') (a, b) (Either r (Maybe a, r'))
indexBy = indexBy_ forkThreadId

---------------------------------------------------------------------------
-- COPIES
---------------------------------------------------------------------------

parList :: ((es :+: es) ~ es, Mergeable es es es) =>
	Sig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
parList = parList_ forkThreadId
