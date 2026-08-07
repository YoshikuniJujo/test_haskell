{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Bech32 (B(..), encode, decode) where

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Char
import Data.Text qualified as T

import Codec.Bech32.Polymod
import Data.Word.Yj
import Tools

data B = B { humanReadPart :: String, dataPart :: [Word5] } deriving (Show, Eq)

encode :: B -> T.Text
encode B { humanReadPart = hrp, dataPart = dp } =
	T.pack $ hrp ++ "1" ++ ((dictChars !!) . fromIntegral <$> (dp ++ cs))
	where cs = word30ToWord5List . polymodL $ hrpToW5s hrp ++ dp

decode :: T.Text -> Either String B
decode = check <=< sepHrpDp . T.unpack

check :: (String, String) -> Either String B
check (h, d) = dict `mapM` d >>= \d' -> case polymodL' $ hrpToW5s h ++ d' of
	1 -> Right B { humanReadPart = h, dataPart = takeR 6 d' }
	_ -> Left "Bech32: Checksum should be 1"

sepHrpDp :: String -> Either String (String, String)
sepHrpDp = (const msg +++ (NE.init `first`)) . spanR (/= '1')
	where msg = "Bech32: no separator '1'"

hrpToW5s :: String -> [Word5]
hrpToW5s ((ord <$>) -> hrp) =
	fromIntegral <$> ((`shiftR` 5) <$> hrp) ++ 0 : ((.&. 0x1f) <$> hrp)

dict :: Char -> Either String Word5
dict = maybe (Left msg) (Right . fromIntegral) . (`L.elemIndex` dictChars)
	where msg = "bad character"

dictChars :: [Char]
dictChars = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
