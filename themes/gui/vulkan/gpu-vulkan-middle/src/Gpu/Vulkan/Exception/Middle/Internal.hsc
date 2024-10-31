{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Gpu.Vulkan.Exception.Middle.Internal where

import Control.Exception
import Control.Exception.Hierarchy
import Data.List.NonEmpty (NonEmpty, nonEmpty)

import Gpu.Vulkan.Exception.Enum

data MultiResult = MultiResult (NonEmpty (Int, Result)) deriving Show

exceptionHierarchy Nothing $ ExNode "E" [
	ExType ''Result,
	ExType ''MultiResult ]

throwUnlessSuccess :: Result -> IO ()
throwUnlessSuccess = throwUnless [Success]

throwUnless :: [Result] -> Result -> IO ()
throwUnless sccs r | r `elem` sccs = pure () | otherwise = throw r

throwUnlessSuccesses :: [Result] -> IO ()
throwUnlessSuccesses = throwUnlesses [Success]

throwUnlesses :: [Result] -> [Result] -> IO ()
throwUnlesses sccs rs = do
	let	irs = zip [0 ..] rs
		irs' = filter ((`notElem` sccs) . snd) irs
	case nonEmpty irs' of
		Nothing -> pure ()
		Just r -> throw $ MultiResult r
