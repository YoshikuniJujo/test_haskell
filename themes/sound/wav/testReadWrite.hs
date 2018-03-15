{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.IO.Unsafe

import qualified Data.ByteString as BS

import ReadWav
import WriteWav
import TransWav

sample2 :: BS.ByteString
sample2 = unsafePerformIO $ BS.readFile "Front_Center.wav"

main :: IO ()
main = do
	let Right w = analyze sample2
	print $ lengthWav w
	BS.writeFile "tmp.wav" . writeWav $ sliceWav w 5700 10000
	BS.writeFile "tmp2.wav" . writeWav $ sliceWav w 58000 68545
