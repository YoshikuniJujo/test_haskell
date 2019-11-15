{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Exts.Heap
import System.IO.Unsafe

main :: IO ()
main = do
	print $ mySum' [123, 456, 789]
	putStrLn ""
	print $ mySum [123, 456, 789]

mySum :: [Int] -> Int
mySum = sm 0
	where
	sm s [] = checkTipe s
	sm s (n : ns) = let s' = (checkTipe s) + n in s' `seq` sm s' ns

mySum' :: [Int] -> Int
mySum' = sm 0
	where
	sm s [] = checkTipe s
	sm s (n : ns) = sm (checkTipe s + n) ns

checkTipe :: a -> a
checkTipe x = unsafePerformIO $ do
	getTipe x >>= print
	return x

getTipe :: a -> IO ClosureType
getTipe = (tipe . info <$>) . getClosureData
