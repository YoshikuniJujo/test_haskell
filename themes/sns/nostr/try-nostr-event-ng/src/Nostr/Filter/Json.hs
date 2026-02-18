{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Filter.Json where

import Data.Maybe
import Data.Vector qualified as V
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Aeson.KeyMap qualified as A
import Nostr.Filter qualified as Filter

import Tools

encode :: Filter.Filter -> A.Value
encode f = A.Object . A.fromList $ catMaybes [
	("ids" ,) . A.Array . V.fromList . (A.String . bsToHexText <$>) <$> Filter.ids f
	]

sampleFilter :: Filter.Filter
sampleFilter = Filter.Filter {
	Filter.ids = Just ["foo", "bar", "baz"],
	Filter.authors = Nothing,
	Filter.kinds = Nothing,
	Filter.tags = [],
	Filter.since = Nothing,
	Filter.until = Nothing,
	Filter.limit = Nothing }
