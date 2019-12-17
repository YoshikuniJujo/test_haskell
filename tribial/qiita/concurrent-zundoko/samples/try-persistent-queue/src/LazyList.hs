{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LazyList where

import Data.Bool
import Data.List

import ShowLazyList
import Printable

newtype LazyList a = LazyList [a]

instance Show a => Printable (LazyList a) where
	show' (LazyList xs) = do
		(e, s) <- showLazyList xs
		pure $ "LazyList [" ++
			intercalate "," s ++ bool ".." "" e ++ "]"
