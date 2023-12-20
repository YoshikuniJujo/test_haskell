{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Data.Ptr where

import Foreign.Ptr

class IsPtr a where
	type Tag a
	toPtr :: a -> Ptr (Tag a)
	fromPtr :: Ptr (Tag a) -> a

data Null = Null

instance IsPtr Null where
	type Tag Null = Null
	toPtr Null = nullPtr
	fromPtr _ = Null
