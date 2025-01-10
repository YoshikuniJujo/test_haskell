{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Swizzle where

import Data.Swizzle.TH

concat <$> swizzle `mapM` [ [a'] | a' <- "xyz" ++ reverse ['a' .. 'w'] ]
concat <$> swizzle `mapM` [ [a', b'] | a' <- "xyzw", b' <- "xyzw" ]
concat <$> swizzle `mapM`
	[ [a', b', c'] | a' <- "xyzw", b' <- "xyzw", c' <- "xyzw"]
concat <$> swizzle `mapM`
	[ [a', b', c', d'] |
		a' <- "xyzw", b' <- "xyzw", c' <- "xyzw", d' <- "xyzw"]
