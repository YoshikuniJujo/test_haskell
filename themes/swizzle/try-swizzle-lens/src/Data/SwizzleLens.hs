{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleLens where

import Data.SwizzleLens.TH

concat <$> swizzleLens `mapM` tail [
	concat [x', y', z', w', u'] |
	x' <- ["", "x"], y' <- ["", "y"],
	z' <- ["", "z"], w' <- ["", "w"], u' <- ["", "u"] ]
