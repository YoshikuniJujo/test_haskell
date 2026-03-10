{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
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
import Data.Aeson.Key qualified as A
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
decode v@(A.Object km) = do
	is <- case km A.!? "ids" of
		Nothing -> pure Nothing
		Just at -> do
			ts <- maybeStringArray at
			pure . Just $ fromHex <$> ts
	as <- case km A.!? "authors" of
		Nothing -> pure Nothing
		Just at -> do
			ts <- maybeStringArray at
			pk <- hexToPubkey `mapM` ts
			pure $ Just pk
	ks <- case km A.!? "kinds" of
		Nothing -> pure Nothing
		Just ai -> do
			ns <- maybeNumberArray ai
			pure $ Just ns
	pure null  {
		Filter.ids = is,
		Filter.authors = as,
		Filter.kinds = ks,
		Filter.tags = lookupTags v
		}
decode _ = Nothing

lookupTags :: A.Value -> [(Char, [T.Text])]
lookupTags v = (`mapMaybe` (['a' .. 'z'] ++ ['A' .. 'Z'])) \c ->
	(c ,) <$> lookupTag v c

lookupTag :: A.Value -> Char -> Maybe [T.Text]
lookupTag (A.Object km) c =
	maybeStringArray =<< km A.!? (A.fromText $ "#" T.:> c)
lookupTag _ _ = Nothing

maybeStringArray :: A.Value -> Maybe [T.Text]
maybeStringArray (A.Array (V.toList -> ts)) = maybeString `mapM` ts
maybeStringArray _ = Nothing

maybeNumberArray :: A.Value -> Maybe [Int]
maybeNumberArray (A.Array (V.toList -> ns)) = maybeNumber `mapM` ns
maybeNumberArray _ = Nothing

maybeString :: A.Value -> Maybe T.Text
maybeString = \case A.String t -> Just t; _ -> Nothing

maybeNumber :: A.Value -> Maybe Int
maybeNumber = \case A.Number n -> Just $ truncate n; _ -> Nothing

null :: Filter.Filter
null = Filter.Filter {
	Filter.ids = Nothing, Filter.authors = Nothing, Filter.kinds = Nothing,
	Filter.tags = [],
	Filter.since = Nothing, Filter.until = Nothing, Filter.limit = Nothing }

addId :: Filter.Filter -> BS.ByteString -> Filter.Filter
addId f@Filter.Filter { Filter.ids = Nothing } i = f { Filter.ids = Just [i] }
addId f@Filter.Filter { Filter.ids = Just is } i =
	f { Filter.ids = Just $ i : is }
