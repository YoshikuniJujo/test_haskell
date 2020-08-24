{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.NoThreadId where

import Control.Moffy.Internal.Sig
import Control.Moffy.Internal.Sig.Type
import Control.Moffy.Internal.React
import Control.Moffy.Internal.React.Type
import Data.Type.Set
import Data.OneOrMore
import Data.Or

infixl 4 `app'`, `iapp'`

app' :: (Mergeable es es es, Semigroup r) =>
	Sig s es (a -> b) r -> Sig s es a r -> Sig s es b r
app' = app_ noForkThreadId

iapp' :: (Mergeable es es es, Semigroup r) =>
	ISig s es (a -> b) r -> ISig s es a r -> ISig s es b r
iapp' = iapp_ noForkThreadId

infixr 8 `first'`

first' :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
first' = first_ noForkThreadId

infixr 7 `at'`

at' :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	React s (es :+: es') (Either r (Maybe a, r'))
at' = at_ noForkThreadId

infixl 7 `break'`, `until'`

break' :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (Maybe a, r'))
break' = break_ noForkThreadId

until' :: Firstable es es' (ISig s (es :+: es') a r) r' =>
	Sig s es a r -> React s es' r' ->
	Sig s (es :+: es') a (Either r (a, r'))
until' = until_ noForkThreadId
