{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.NoThreadId (
	-- * Applicative
	app, iapp,
	-- * Parallel
	first, at, break, until, indexBy,
	-- * Copies
	parList ) where

import Prelude hiding (break, until)
import Control.Moffy.Internal.Sig (
	app_, iapp_, at_, break_, until_, indexBy_, parList_ )
import Control.Moffy.Internal.Sig.Type (Sig, ISig)
import Control.Moffy.Internal.React (Firstable, first_)
import Control.Moffy.Internal.React.Type (React, noThreadId)
import Data.Type.Set ((:+:))
import Data.OneOrMore (Mergeable)
import Data.Or (Or)

---------------------------------------------------------------------------

-- * APPLICATIVE
-- * PARALLEL
-- * COPIES

---------------------------------------------------------------------------
-- APPLICATIVE
---------------------------------------------------------------------------

infixl 4 `app`, `iapp`

app :: ((es :+: es) ~ es, Mergeable es es es, Monoid r) =>
	Sig s es (a -> b) r -> Sig s es a r -> Sig s es b r
app = app_ noThreadId

iapp :: ((es :+: es) ~ es, Mergeable es es es, Semigroup r) =>
	ISig s es (a -> b) r -> ISig s es a r -> ISig s es b r
iapp = iapp_ noThreadId

---------------------------------------------------------------------------
-- PARALLEL
---------------------------------------------------------------------------

infixr 8 `first`

first :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
first = first_ noThreadId

infixr 7 `at`

at :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	React s (es :+: es') (Either r (Maybe a, r'))
at = at_ noThreadId

infixl 7 `break`, `until`

break :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
break = break_ noThreadId

until :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (a, r'))
until = until_ noThreadId

infixl 7 `indexBy`

indexBy ::
	Firstable es es' (ISig s (es :+: es') a r) (ISig s (es :+: es') b r') =>
	Sig s es a r -> Sig s es' b r' ->
	Sig s (es :+: es') (a, b) (Either r (Maybe a, r'))
indexBy = indexBy_ noThreadId

---------------------------------------------------------------------------
-- COPIES
---------------------------------------------------------------------------

parList :: ((es :+: es) ~ es, Mergeable es es es) =>
	Sig s es (ISig s es a r) r' -> Sig s es [a] ([r], r')
parList = parList_ noThreadId
