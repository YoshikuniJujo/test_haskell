{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module A (Test(..)) where

class Test a b where
	test :: a -> b -> String

instance Test String a where
	test str _ = str
