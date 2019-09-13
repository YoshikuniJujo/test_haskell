{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Foreign.C.String
import System.IO

import VectoredIo
import Iovec

main :: IO ()
main = main1 >> main2 >> main3

str1, str2 :: [CChar]
str1 = castCharToCChar <$> "Hello, "
str2 = castCharToCChar <$> "World!\n"

main1 :: IO ()
main1 = do
	withFile "foo.txt" WriteMode $ \h ->
		writeVector h [str1, str2]
	withFile "foo.txt" ReadMode $ \h -> do
		Right [s1, s2] <- readVector h [7, 7]
		print (castCCharToChar <$> s1 :: String)
		print (castCCharToChar <$> s2 :: String)

main2 :: IO ()
main2 = do
	withFile "foo.txt" WriteMode $ \h ->
		writeVector h [[123 :: Int, 456], [789]]
	withFile "foo.txt" ReadMode $ \h -> do
		Right [ns1, ns2] <- readVector h [2, 1]
		print (ns1 :: [Int])
		print ns2

main3 :: IO ()
main3 = do
	withFile "foo.txt" WriteMode $ \h ->
		writeVector h (str1 :. [123 :: Int, 456] :. ListTupleNil)
	withFile "foo.txt" ReadMode $ \h -> do
		Right (s1 :. ns1 :. ListTupleNil :: ListTuple [CChar, Int]) <-
			readVector h [7, 2]
		print $ castCCharToChar <$> s1
		print ns1
