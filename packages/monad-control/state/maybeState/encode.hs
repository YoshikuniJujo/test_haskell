{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Bits (shiftR)
import qualified Data.ByteString as BS

import qualified YjChunks as Yc
import CRC (calc)

main :: IO ()
main = BS.writeFile "hello.yjcs" sample

sample :: BS.ByteString
Just sample = encode <$> mapM (uncurry Yc.yjChunk) [("YSJJ", "hello")]

encode :: Yc.YjChunks -> BS.ByteString
encode = (Yc.magic `BS.append`) . BS.concat . map encode1 . (++ [Yc.dend])

encode1 :: Yc.YjChunk -> BS.ByteString
encode1 = uncurry BS.append . first (le 4) . Yc.encode (le 4 . calc)
	where
	le 0 _ = ""
	le c n = BS.cons (fromIntegral n) $ le (c - 1 :: Int) (n `shiftR` 8)
