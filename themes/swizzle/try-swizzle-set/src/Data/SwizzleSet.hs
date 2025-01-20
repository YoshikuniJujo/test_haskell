{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleSet where

import Data.SwizzleSet.TH

concat <$> (swizzleSet "" . (: "")) `mapM` ("xyz" ++ reverse ['a' .. 'w'])

concat <$> swizzleSet "" `mapM` filter ((> 1) . length) [
	concat [a', b', c', d', e'] |
	a' <- ["", "x"], b' <- ["", "y"],
	c' <- ["", "z"], d' <- ["", "w"], e' <- ["", "v"] ]
