{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy (
	-- * Type
	Sig, ISig, React, Rct, EvReqs, EvOccs, Request(..),
	-- * Constraint
	Firstable, Adjustable,
	-- * Combinator
	-- ** Await and Adjust
	await, adjust, adjustSig,
	-- ** Simple Sig
	emit, waitFor, repeat,
	-- ** Traverse
	find, scanl,
	-- ** Parallel
	first, at, break, until, indexBy,
	-- ** Copies
	spawn, parList ) where

import Prelude hiding (repeat, scanl, until, break)

import Control.Moffy.Internal.Sig (
	adjustSig, at, break, until, indexBy, spawn, parList )
import Control.Moffy.Internal.Sig.Type (
	Sig, ISig, emit, waitFor, repeat, find, scanl )
import Control.Moffy.Internal.React (Firstable, Adjustable, first_, adjust)
import Control.Moffy.Internal.React.Type (
	React, Rct, EvReqs, EvOccs, Request(..), await, forkThreadId )
import Data.Type.Set ((:+:))
import Data.Or (Or)

infixr 8 `first`

first :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
first = first_ forkThreadId
