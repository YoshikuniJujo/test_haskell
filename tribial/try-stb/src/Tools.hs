{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Arrow
import Data.List
import Data.Word

import qualified Data.ByteString as BS

compareN :: Int -> BS.ByteString -> BS.ByteString -> Bool
compareN n b c = BS.take n b == BS.take n c

subAnd :: BS.ByteString -> BS.ByteString -> [(Word8, Int)]
subAnd b c = map (head &&& length) . group . sort . filter (/= 0) $ zipWith subtract (BS.unpack b) (BS.unpack c)
