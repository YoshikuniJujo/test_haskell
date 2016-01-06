{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.List
import Data.Word
import Data.Char
import qualified Data.ByteString as BS

main :: IO ()
main = interact id

decode :: String -> BS.ByteString
decode ('%' : a : b : cs) = (hex [a, b]) `BS.cons` decode cs
decode (c : cs) = fromIntegral (ord c) `BS.cons` decode cs
decode _ = ""

hex :: String -> Word8
hex = hx . reverse
	where
	hx (c : cs) = fromIntegral (fromJust $ c `elemIndex` digits) + 16 * hx cs
	hx _ = 0

digits :: String
digits = "0123456789ABCDEF"
