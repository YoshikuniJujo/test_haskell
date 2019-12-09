{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LazyList where

import Data.Bool
import Data.List
import System.IO.Unsafe

import ShowLazyList

data LazyList a = LazyList [a]

instance Show a => Show (LazyList a) where
	show (LazyList xs) = unsafePerformIO $ do
		(e, s) <- showLazyList xs
		pure $ "LazyList [" ++ intercalate "," s ++ bool ".." "" e ++ "]"

atoz :: [Char]
atoz = ['a' .. 'z']

foo, foo', foo'' :: Int
foo = length . show $ LazyList atoz
foo' = length . show . LazyList $! atoz
-- foo'' = length atoz `seq` (length $! (show $ LazyList atoz))
foo'' = length $! (show $ length atoz `seq` LazyList atoz)

bar :: Bool -> Int
bar True = length . show $ LazyList atoz
bar False = length . show $ LazyList atoz

baz :: (String -> LazyList Char) -> String -> Int
baz l a = length . show $ l a
