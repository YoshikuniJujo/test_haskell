{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree (

	-- * BYTE STRING

	ByteString,
	pattern Empty, pattern (:<), pattern (:>), pattern (:<|), pattern (:|>),

	-- * INTRODUCING AND ELIMINATING

	empty, singleton, pack, unpack, fromStrict, toStrict,

	-- * BASIC INTERFACE

	cons, snoc, append, uncons, unsnoc, null, length,

	-- * REDUCING BYTESTRINGS (FOLDS)

	foldl',

	-- * GENERATING

	replicate,

	-- * BREAKING

	splitAt', span

) where

import Prelude hiding (concat, null, length, replicate, span, foldl')
import Control.Arrow
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.FingerTree.Internal

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span _ Empty = (Empty, Empty)
span p ba@(b :< bs)
	| p b = (b :<) `first` span p bs
	| otherwise = (Empty, ba)

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f !v = \case Empty -> v; bs :<| bss -> foldl' f (BS.foldl' f v bs) bss
