module Main where

import System.Environment
import qualified Data.ByteString as BS

import Chunks
import ReadPng
import Tools

main :: IO ()
main = do
	fp : _ <- getArgs
	png <- BS.readFile fp
	let	c = fst $ (checkMagic >> (,,) <$> chunkOther <*> chunkOther <*> chunkOther) `runReadPng` png
	print $ fst' <$> c
	putStrLn . take 100 . show $ snd' <$> c
	print $ third <$> c
