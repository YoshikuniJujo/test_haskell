{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data2.SwizzleSet where

import Data2.SwizzleSet.TH

concat <$> (swizzleSet "" . (: "")) `mapM` ("xyz" ++ reverse ['a' .. 'w'])

concat <$> swizzleSet "" `mapM` filter ((> 1) . length) [
	concat [a', b', c', d', e'] |
	a' <- ["", "x"], b' <- ["", "y"],
	c' <- ["", "z"], d' <- ["", "w"], e' <- ["", "v"] ]
