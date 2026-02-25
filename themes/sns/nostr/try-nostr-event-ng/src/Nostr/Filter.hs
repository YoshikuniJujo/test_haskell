{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Filter where

import Prelude hiding (until)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

data Filter = Filter {
	ids :: Maybe [BS.ByteString],
	authors :: Maybe [Pub],
	kinds :: Maybe [Int],
	tags :: [(Char, [T.Text])],
	since :: Maybe UnixTime, until :: Maybe UnixTime,
	limit :: Maybe Int }
	deriving (Show, Eq)

instance Ord Filter where
	f1 <= f2 = toFilterOrd f1 <= toFilterOrd f2

data FilterOrd = FilterOrd {
	idsOrd :: Maybe [BS.ByteString],
	authorsOrd :: Maybe [BS.ByteString],
	kindsOrd :: Maybe [Int],
	tagsOrd :: [(Char, [T.Text])],
	sinceOrd :: Maybe UnixTime, untilOrd :: Maybe UnixTime,
	limitOrd :: Maybe Int }
	deriving (Show, Eq, Ord)

toFilterOrd :: Filter -> FilterOrd
toFilterOrd Filter {
	ids = i, authors = a, kinds = k, tags = t, since = s, until = u,
	limit = l } = FilterOrd {
	idsOrd = i, authorsOrd = (serialize_point <$>) <$> a,
	kindsOrd = k, tagsOrd = t, sinceOrd = s, untilOrd = u, limitOrd = l }
