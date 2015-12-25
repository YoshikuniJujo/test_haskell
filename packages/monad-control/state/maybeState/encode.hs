{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString as BS

import YjChunks
import CRC2

main :: IO ()
main = BS.writeFile "hello.yjcs" sample

sample :: BS.ByteString
Just sample = encode <$> mapM (uncurry mkYjChunk) [("YSJJ", "hello")]

encode :: YjChunks -> BS.ByteString
encode = (magic `BS.append`) . BS.concat . map encode1 . (++ [dend])

encode1 :: YjChunk -> BS.ByteString
encode1 yc = let (l, bs) = toByteString (fromNum 4 . crc) yc in fromNum 4 l `BS.append` bs
