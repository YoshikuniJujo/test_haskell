{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGe FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.Sig (
	-- * Types
	Sig, ISig,
	-- * Run Sig
	interpretSig,
	-- * Conversion
	cur, emit, always, waitFor,
	-- * Transformation
	map, scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel composition
	at, until, (<^>), indexBy
	) where

import Prelude hiding (map, scanl, repeat, until)

import MonadicFrp.Sig.Internal
