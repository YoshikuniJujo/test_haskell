{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Item (item, byte) where

import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.Word
import qualified Data.ByteString as BS

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)

byte :: BS.ByteString -> Maybe Word8
byte bs = case BS.uncons bs of Just (w, "") -> Just w; _ -> Nothing
