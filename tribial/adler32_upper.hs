{-# OPTIONS_GHC -fno-warn-tabs #-}

import Control.Arrow
import Data.List
import Data.Bits
import Data.Bool
import Data.Word

import qualified Data.ByteString as BS

upper :: Word32
upper = maxBound - fromIntegral (maxBound :: Word8)

add :: Integral a => Word32 -> a -> Word32
add w1 w2 = let w = w1 + fromIntegral w2 in
	bool w (w `mod` 65521) (w > upper)
-- add w1 w2 = (w1 + fromIntegral w2) `mod` 65521

adler32 :: BS.ByteString -> BS.ByteString
adler32 bs = BS.pack $ map fromIntegral [
	b `shiftR` 8, b .&. 0xff, a `shiftR` 8, a .&. 0xff ]
	where
	(b, a) = foldl' (flip $ \k -> (`add` k) *** const k) (0, 0)
		. tail $ scanl' (\w -> add w) 1 $ BS.unpack bs
