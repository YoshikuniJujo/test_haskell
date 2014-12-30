{-# LANGUAGE TypeFamilies #-}

module DecodeAsn1Common (decode1) where

import Control.Applicative
import Data.Word8

import qualified Data.ByteString as BS

import Analyzer
import qualified ListLike as LL

data Asn1 = Asn1 Asn1Tag BS.ByteString deriving Show

data Asn1Tag = Asn1Tag TagClass DataClass Integer deriving Show

data TagClass = Universal | Application | ContextSpecific | Private deriving Show

data DataClass = Primitive | Constructed deriving Show

decode1 :: (LL.ListLike a, LL.Element a ~ Word8) => Analyzer a Asn1
decode1 = Asn1 <$> decodeTag <*> decodeContents

decodeTag :: (LL.ListLike a, LL.Element a ~ Word8) => Analyzer a Asn1Tag
decodeTag = undefined

decodeContents :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a BS.ByteString
decodeContents = undefined
