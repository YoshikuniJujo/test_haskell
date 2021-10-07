{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import System.FilePath
import Codec.Picture

import qualified Data.ByteString as BS

main :: IO ()
main = do
	fp : w : h : _ <- getArgs
	putStrLn $ replaceExtension fp "png"
	cnt <- BS.readFile fp
	print $ BS.length cnt
	print . BS.unpack . BS.take 100 $ BS.dropWhile (== 0) cnt
	writePng (replaceExtension fp "png")
		$ generateImage (ind cnt $ read w) (read w) (read h)

ind :: BS.ByteString -> Int -> Int -> Int -> PixelRGBA8
ind bs w x y = PixelRGBA8
	(bs `BS.index` base) (bs `BS.index` (base + 1)) (bs `BS.index` (base + 2))
	(bs `BS.index` (base + 3))
	where base = (x + y * w) * 4
