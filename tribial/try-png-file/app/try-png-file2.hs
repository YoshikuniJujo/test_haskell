{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Typeable
import Data.List qualified as L
import System.Environment
import qualified Data.ByteString as BS

import Chunks
import Chunks.SomeChunk
import Chunks.Core
import ReadPng
import Tools

main :: IO ()
main = do
	fp : _ <- getArgs
	png <- BS.readFile fp
	let	ck = fst $ (checkMagic >> (,,) <$> chunkOther <*> chunkOther <*> chunkOther) `runReadPng` png
	print $ fst' <$> ck
	putStrLn . take 100 . show $ snd' <$> ck
	print $ third <$> ck
--	print $ chunkName . snd' <$> ck
	case ck of
		Left err -> putStrLn err
		Right r -> do
			fromSomeChunk (fst' r) \c -> print $ typeOf c
			fromSomeChunk (fst' r) \c ->
				print $ typeOf c == typeRep (Proxy :: Proxy Ihdr)
			fromSomeChunk (fst' r) \c ->
				case cast c of
					Nothing -> putStrLn "not IHDR"
					Just i -> print $ ihdrWidth i
			fromSomeChunk (snd' r) \c -> print $ typeOf c
	print $ L.sort [
		typeRep (Proxy :: Proxy OtherChunk),
		typeRep (Proxy :: Proxy Ihdr),
		typeRep (Proxy :: Proxy (Maybe Int -> Int))
		]
