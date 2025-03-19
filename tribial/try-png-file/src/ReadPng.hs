{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ReadPng where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Bits
import Data.ByteString qualified as BS

type ReadPng = ExceptT String (State BS.ByteString)

runReadPng :: ReadPng a -> BS.ByteString -> (Either String a, BS.ByteString)
runReadPng = runState . runExceptT

pop :: Int -> ReadPng BS.ByteString
pop n = lift $ gets (BS.take n) <* modify (BS.drop n)

checkMagic :: ReadPng ()
checkMagic = do
	m <- pop 8
	when (m /= magic) $ throwError "not PNG file"

magic :: BS.ByteString
magic = "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a"

dataLength :: ReadPng Int
dataLength = bsToNum32 <$> pop 4

bsToNum32 :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum32 bs
	| BS.length bs == 4 = bigEndian 0 . (fromIntegral <$>) $ BS.unpack bs
	| otherwise = error "bad"

bigEndian :: Bits n => n -> [n] -> n
bigEndian s [] = s
bigEndian s (n : ns) = bigEndian (s `shiftL` 8 .|. n) ns

getChunkName :: ReadPng BS.ByteString
getChunkName = pop 4
