{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckWav (wavData1, wavData2, wavData3, wavData4) where

import ReadWav

import Data.Either
import Data.Int
import qualified Data.ByteString as BS
import System.IO.Unsafe

sample1, sample2, sample3, sample4 :: BS.ByteString
sample1 = unsafePerformIO $ BS.readFile "Noise.wav"
sample2 = unsafePerformIO $ BS.readFile "Front_Center.wav"
sample3 = unsafePerformIO $ BS.readFile "tmp.wav"
sample4 = unsafePerformIO $ BS.readFile "tmp2.wav"

wavData1, wavData2, wavData3, wavData4 :: [Int16]
wavData1 = wavData (fromRight undefined $ analyze sample1)
wavData2 = wavData (fromRight  undefined $ analyze sample2)
wavData3 = wavData (fromRight  undefined $ analyze sample3)
wavData4 = wavData (fromRight  undefined $ analyze sample4)
