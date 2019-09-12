{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.String
import Foreign.C.Types
import System.IO
import Numeric

import VectoredIo
import Iovec

main :: IO ()
main = main1 >> main2 >> main3

main1 :: IO ()
main1 = do
	withFile "tmp_hl.txt" WriteMode $ \h ->
		writeVector h ([0x3132333435363738 :: Int] :. (castCharToCChar <$> "w\n") :. ListTupleNil)
	withFile "tmp_hl.txt" ReadMode $ \h -> do
		Right (n :. str :. ListTupleNil :: ListTuple [Int, CChar]) <- readVector h [1, 2]
		mapM_ (putStrLn . ("0x" ++)) $ (`showHex` "") <$> n
		print $ castCCharToChar <$> str

main2 :: IO ()
main2 = do
	withFile "tmp_hl2.txt" WriteMode $ \h ->
		writeVector h [castCharToCChar <$> "Hello, ", castCharToCChar <$> "world!\n"]
	withFile "tmp_hl2.txt" ReadMode $ \h -> do
		Right [s1, s2] <- readVector h [7, 7]
		print (castCCharToChar <$> s1 :: String)
		print (castCCharToChar <$> s2 :: String)

main3 :: IO ()
main3 = withFile "dummy.txt" ReadMode $ \h -> do
	Right [i1 :: [Int], i2] <- readVector h [1, 1]
	print $ (`showHex` "") <$> i1
	print $ (`showHex` "") <$> i2
