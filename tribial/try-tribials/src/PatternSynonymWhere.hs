{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PatternSynonymWhere where

newtype Foo = Foo Int deriving Show

pattern Mul2 :: Int -> Foo
pattern Mul2 n <- Foo (div2 -> Just n)
	where
	Mul2 n = Foo $ 2 * n

div2 :: Int -> Maybe Int
div2 n = if even n then Just $ n `div` 2 else Nothing
