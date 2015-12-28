{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Item (item, byte, untilM) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import "monads-tf" Control.Monad.State (StateT, gets, put)
import Data.Bool (bool)
import Data.Word (Word8)
import qualified Data.ByteString as BS (ByteString, uncons, splitAt)

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)

byte :: BS.ByteString -> Maybe Word8
byte bs = case BS.uncons bs of Just (w, "") -> Just w; _ -> Nothing

untilM :: (Monad m, Functor m) => (a -> Bool) -> m a -> m [a]
untilM p m = bool (return []) <$> ((<$> untilM p m) . (:)) <*> not . p =<< m
