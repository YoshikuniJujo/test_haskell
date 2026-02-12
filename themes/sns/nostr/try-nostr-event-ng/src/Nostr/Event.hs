{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Event where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

import Bech32

data E = E {
	pubkey :: Pub,
	created_at :: UnixTime,
	kind :: Int,
	tags :: [(T.Text, (T.Text, [T.Text]))],
	content :: T.Text }
	deriving Show

sample :: FilePath -> IO E
sample fp = do
	Just pub <- dataPart . chomp <$> T.readFile fp
	Just pk <- pure $ parse_point pub
	ut <- getUnixTime
	pure E {
		pubkey = pk,
		created_at = ut,
		kind = 1,
		tags = [],
		content = "Hello" }

chomp :: T.Text -> T.Text
chomp t = if T.last t == '\n' then T.init t else t
