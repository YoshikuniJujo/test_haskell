{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp (
	-- * Types
	Sig, ISig, React, EvReqs, EvOccs, Request(..),
	Firstable, CollapsableOccurred,
	-- * React
	await, adjust, first,
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel composition
	at, until, indexBy,
	) where

import Prelude hiding (map, repeat, scanl, until)

import MonadicFrp.Sig
import MonadicFrp.React
