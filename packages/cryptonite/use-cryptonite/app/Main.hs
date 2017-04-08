module Main where

import Crypto.MAC.HMAC
import Crypto.Hash.Algorithms
import Data.ByteString.Base64.URL
import Data.ByteArray

import Crypto.Random

import qualified Data.ByteString as BS

import Lib

main :: IO ()
main = do
	print . encode . convert $ hmacGetDigest (hmac
		("abc" :: BS.ByteString)
		("def" :: BS.ByteString) :: HMAC SHA256)
