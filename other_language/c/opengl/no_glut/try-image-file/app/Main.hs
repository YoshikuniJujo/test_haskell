{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Codec.Picture

import qualified Data.ByteString as BS

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	print $ BS.length cnt
	print . BS.unpack . BS.take 100 $ BS.dropWhile (== 0) cnt
	writePng "foo.png"
		$ generateImage (ind cnt) 512 512

ind :: BS.ByteString -> Int -> Int -> PixelRGBA8
ind bs x y = PixelRGBA8
	(bs `BS.index` base) (bs `BS.index` (base + 1)) (bs `BS.index` (base + 2))
	(bs `BS.index` (base + 3))
	where base = (x + y * 512) * 4
