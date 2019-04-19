{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (divideBase58, xorBase58) where

import Data.Bits
import Data.Function
import Crypto.Random
import Base58.BitcoinFlavor

import qualified Data.ByteString as BS

divideBase58 :: String -> IO (String, String)
divideBase58 a1 = do
	b1 <- maybe undefined return $ decode a1
	b2 <- getRandomBytes $ BS.length b1
	let	a2 = encode b2
	a3 <- maybe undefined return $ xorBase58 a1 a2
	return (a2, a3)

xorBase58 :: String -> String -> Maybe String
xorBase58 a1 a2 = do
	b1 <- decode a1
	b2 <- decode a2
	b3 <- xorBytes b1 b2
	return $ encode b3

xorBytes :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
xorBytes b1 b2
	| BS.length b1 == BS.length b2 =
		Just . BS.pack $ on (zipWith xor) BS.unpack b1 b2
	| otherwise = Nothing
