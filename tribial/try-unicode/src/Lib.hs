module Lib where

import Data.Word
import Numeric

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

sample :: IO BS.ByteString
sample = BS.readFile "aiu.txt"

putSample :: IO ()
putSample = BSC.putStr =<< sample

unpackedSample :: IO [Word8]
unpackedSample = BS.unpack <$> sample

hexedSample :: IO [String]
hexedSample = ((`showHex` "") <$>) <$> unpackedSample
