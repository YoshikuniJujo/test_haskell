{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Try.TH

concat <$> mkXy `mapM` (filter ((> 0) . length) [
	concat [x', y', z', w', v'] |
	x' <- ["", "x"], y' <- ["", "y"],
	z' <- ["", "z"], w' <- ["", "w"], v' <- ["", "v"] ])
