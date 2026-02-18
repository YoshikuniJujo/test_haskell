{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Samples where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1
import Nostr.Event
import Nostr.Filter qualified as Filter
import Bech32

sample :: FilePath -> IO E
sample fp = do
	Just pk <- readPub fp
	ut <- getUnixTime
	pure E {
		pubkey = pk,
		created_at = ut,
		kind = 1,
		tags = [],
		content = "Hello" }

readPub :: FilePath -> IO (Maybe Pub)
readPub fp = (parse_point =<<) . dataPart . chomp <$> T.readFile fp

chomp :: T.Text -> T.Text
chomp t = if T.last t == '\n' then T.init t else t

sampleFilter :: Filter.Filter
sampleFilter = Filter.Filter {
	Filter.ids = Just ["foo", "bar", "baz"],
	Filter.authors = Nothing,
	Filter.kinds = Nothing,
	Filter.tags = [],
	Filter.since = Nothing,
	Filter.until = Nothing,
	Filter.limit = Nothing }
