{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TextLike (TextUtf8, utf8) where

import Foreign.C.Types
import Codec.Binary.UTF8.String

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

class TextUtf8 t where utf8 :: t -> [CChar]

instance TextUtf8 c => TextUtf8 [c] where utf8 = (utf8 =<<)
instance TextUtf8 Char where utf8 = (CChar . fromIntegral <$>) . encodeChar

instance TextUtf8 T.Text where
	utf8 = (CChar . fromIntegral <$>) . BS.unpack . T.encodeUtf8
