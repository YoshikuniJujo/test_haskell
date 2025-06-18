{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree (

	-- * BYTE STRING

	ByteString, pattern Empty, pattern (:<|), pattern (:|>),

	-- * INTRODUCING AND ELIMINATING

	empty, singleton, pack, unpack, fromStrict, toStrict,

	-- * BASIC INTERFACE

	cons, snoc, append, uncons, unsnoc, null, length,

	-- * GENERATING

	replicate,

	-- * BREAKING

	splitAt'

) where

import Prelude hiding (concat, null, length, replicate)
import Data.ByteString.FingerTree.Internal
