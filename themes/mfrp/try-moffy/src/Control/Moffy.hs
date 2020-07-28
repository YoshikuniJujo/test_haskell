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

import Control.Moffy.Internal.Sig (adjustSig, spawn, parList, at, break, until, indexBy)
import Control.Moffy.Internal.Sig.Type (
	Sig, ISig, emit, waitFor, scanl, find, repeat )
import Control.Moffy.Internal.React (Firstable, Adjustable, adjust, first)
import Control.Moffy.Internal.React.Type (
	React, Rct, EvReqs, EvOccs, Request(..), await )
