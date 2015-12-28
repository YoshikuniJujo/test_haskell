{-# LANGUAGE PackageImports #-}

module Item (item) where

import Control.Arrow
import "monads-tf" Control.Monad.State
import qualified Data.ByteString as BS

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)
