{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Filter.Json where

import Prelude hiding (null)
import Foreign.C.Types
import Data.Maybe
import Data.Vector qualified as V
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.String
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.UnixTime
import Crypto.Curve.Secp256k1
import Nostr.Filter qualified as Filter

import Tools

encode :: Filter.Filter -> A.Value
encode f = A.Object . A.fromList $ catMaybes [
	("ids" ,) . A.Array . V.fromList
		. (A.String . bsToHexText <$>) <$> Filter.ids f,
	("authors" ,) . A.Array . V.fromList
		. (A.String . bsToHexText . BS.tail . serialize_point <$>)
		<$> Filter.authors f,
	("kinds" ,) . A.Array . V.fromList
		. (A.Number . fromIntegral <$>) <$> Filter.kinds f ] ++
	(uncurry tagToValue <$> Filter.tags f) ++ catMaybes [
	("since" ,) . A.Number . fromIntegral
		. (\(CTime ct) -> ct) . utSeconds <$> Filter.since f,
	("until" ,) . A.Number . fromIntegral
		. (\(CTime ct) -> ct) . utSeconds <$> Filter.until f,
	("limit" ,) . A.Number . fromIntegral <$> Filter.limit f ]

tagToValue :: Char -> [T.Text] -> (A.Key, A.Value)
tagToValue k vs = (fromString ['#', k], A.Array . V.fromList $ A.String <$> vs)

decode :: A.Value -> Maybe Filter.Filter
decode (A.Object km) = do
	is <- case km A.!? "ids" of
		Nothing -> pure Nothing
		Just at -> do
			ts <- maybeStringArray at
			pure . Just $ fromHex <$> ts
	pure null  {
		Filter.ids = is
		}
decode _ = Nothing

maybeStringArray :: A.Value -> Maybe [T.Text]
maybeStringArray (A.Array (V.toList -> ts)) = maybeString `mapM` ts
maybeStringArray _ = Nothing

maybeString :: A.Value -> Maybe T.Text
maybeString (A.String t) = Just t
maybeString _ = Nothing

null :: Filter.Filter
null = Filter.Filter {
	Filter.ids = Nothing, Filter.authors = Nothing, Filter.kinds = Nothing,
	Filter.tags = [],
	Filter.since = Nothing, Filter.until = Nothing, Filter.limit = Nothing }

addId :: Filter.Filter -> BS.ByteString -> Filter.Filter
addId f@Filter.Filter { Filter.ids = Nothing } i = f { Filter.ids = Just [i] }
addId f@Filter.Filter { Filter.ids = Just is } i =
	f { Filter.ids = Just $ i : is }
