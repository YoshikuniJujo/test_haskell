{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp (
	-- * Types
	Sig, ISig, React, EvReqs, EvOccs, Request(..),
	Adjustable, Firstable, CollapsableOccurred,
	-- * React
	await, adjust, first,
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel composition
	at, break, until, indexBy,
	) where

import Prelude hiding (repeat, scanl, break, until)

import MonadicFrp.Sig (
	Sig, ISig, emit, waitFor, scanl, find, repeat, spawn, parList,
	at, break,  until, indexBy )
import MonadicFrp.React (
	React, EvReqs, EvOccs, Request(..),
	Adjustable, Firstable, CollapsableOccurred, await, adjust, first )
