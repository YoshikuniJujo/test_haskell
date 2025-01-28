{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleLens where

import Data.SwizzleLens.TH

concat <$> (swizzleLens "" . (: "")) `mapM` ("xyz" ++ reverse ['a' .. 'w'])

concat <$> swizzleLens "" `mapM` (filter ((> 1) . length) [
	concat [x', y', z', w', v'] |
	x' <- ["", "x"], y' <- ["", "y"],
	z' <- ["", "z"], w' <- ["", "w"], v' <- ["", "v"] ])
