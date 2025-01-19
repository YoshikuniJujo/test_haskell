{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}

module Data.SwizzleModify where

import Data.SwizzleModify.TH

concat <$> swizzleModify `mapM` (tail [
	concat [x', y', z', w', v'] |
	x' <- ["", "x"], y' <- ["", "y"],
	z' <- ["", "z"], w' <- ["", "w"], v' <- ["", "v"] ])
