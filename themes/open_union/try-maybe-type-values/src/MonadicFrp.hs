{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp (
	-- * Types
	Sig, ISig, React, EvReqs, EvOccs, Request(..),
	Adjustable, Firstable, CollapsableOccurred,
	-- * React
	await, await', adjust, first,
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel composition
	at, break, until, indexBy,
	) where

import Prelude hiding (map, repeat, scanl, break, until)

import MonadicFrp.Sig
import MonadicFrp.React
