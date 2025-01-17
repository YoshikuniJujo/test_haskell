{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleSet where

import Data.SwizzleSet.TH

concat <$> swizzleSet `mapM` tail [
	concat [a', b', c', d', e'] |
	a' <- ["", "x"], b' <- ["", "y"],
	c' <- ["", "z"], d' <- ["", "w"], e' <- ["", "v"] ]
