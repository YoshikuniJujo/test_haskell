{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Vulkan.Exception where

import Control.Exception
import Control.Exception.Hierarchy
import Data.List.NonEmpty (NonEmpty, nonEmpty)

import Vulkan.Exception.Enum

data MultiResult = MultiResult (NonEmpty (Int, Result)) deriving Show

exceptionHierarchy Nothing $ ExNode "E" [
	ExType ''Result,
	ExType ''MultiResult ]

throwUnlessSuccess :: Result -> IO ()
throwUnlessSuccess = \case Success -> pure (); e -> throw e

throwUnlessSuccesses :: [Result] -> IO ()
throwUnlessSuccesses rs = do
	let	irs = zip [0 ..] rs
		irs' = filter ((/= Success) . snd) irs
	case nonEmpty irs' of
		Nothing -> pure ()
		Just r -> throw $ MultiResult r
