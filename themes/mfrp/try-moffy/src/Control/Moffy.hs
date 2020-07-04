{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy (
	-- * Types
	Sig, ISig, React, EvReqs, EvOccs, Request(..), Firstable, Adjustable,
	-- * React
	await, adjust, first,
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel Composition
	at, break, until, indexBy
	) where

import Prelude hiding (repeat, scanl, until, break)

import Control.Moffy.Internal.Sig
import Control.Moffy.Internal.Sig.Common
import Control.Moffy.Internal.React
import Control.Moffy.Internal.React.Type
