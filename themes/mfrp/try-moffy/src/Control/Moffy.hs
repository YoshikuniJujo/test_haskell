{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy (
	-- * Types
	Sig, ISig, React, Rct, EvReqs, EvOccs, Request(..),
	Firstable, Adjustable,
	-- * React
	await, adjust, first,
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel Composition
	at, break, until, indexBy ) where

import Prelude hiding (repeat, scanl, until, break)

import Control.Moffy.Internal.Sig (spawn, parList, at, break, until, indexBy)
import Control.Moffy.Internal.Sig.Type (
	Sig, ISig, emit, waitFor, scanl, find, repeat )
import Control.Moffy.Internal.React (Firstable, Adjustable, adjust, first)
import Control.Moffy.Internal.React.Type (
	React, Rct, EvReqs, EvOccs, Request(..), await )
