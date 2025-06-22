{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
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

	-- * GENERATING

	replicate,

	-- * BREAKING

	splitAt', span

) where

import Prelude hiding (concat, null, length, replicate, span)
import Control.Arrow
import Data.Word
import Data.ByteString.FingerTree.Internal

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p ba@(b :< bs)
	| p b = (b :<) `first` span p bs
	| otherwise = (Empty, ba)
