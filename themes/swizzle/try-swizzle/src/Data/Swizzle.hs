{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Swizzle where

import Data.Swizzle.TH

concat <$> swizzle `mapM` reverse [ [a, b] | a <- "xyz", b <- "xyz" ]
concat <$> swizzle `mapM` reverse
	[ [a, b, c] | a <- "xyz", b <- "xyz", c <- "xyz"]
