{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event.Json (encode, decode) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.UnixTime
import Crypto.Hash.SHA256
import Crypto.Curve.Secp256k1
import Numeric

import Event qualified as Event
import Tools

decode :: A.Object -> Maybe Event.E
decode obj = do
	A.String pk <- A.lookup "pubkey" obj
	crat <- fromScientific <$> A.lookup "created_at" obj
	knd <- fromScientific <$> A.lookup "kind" obj
	tgs <- A.lookup "tags" obj
	cnt <- (\(A.String s) -> s) <$> A.lookup "content" obj
	A.String sig <- A.lookup "sig" obj
	let	srzd = Event.serialize pk crat knd tgs cnt
		hshd = hash $ BSU.fromString srzd
	pk' <- parse_point . BS.pack . (fst . head . readHex <$>) . separate 2 $ T.unpack pk
	let	sig' = BS.pack . (fst . head . readHex <$>) . separate 2 $ T.unpack sig
	guard $ verify_schnorr hshd pk' sig'
	pure Event.E {
		Event.pubkey = pk',
		Event.created_at = fromEpochTime . CTime $ fromIntegral crat,
		Event.kind = knd,
		Event.tags = decodeTags tgs,
		Event.content = cnt }

decodeTags :: A.Value -> Map.Map T.Text (T.Text, [T.Text])
decodeTags (A.Array (toList -> ts)) = Map.fromList $ decodeTags1 <$> ts
decodeTags _ = error "bad"

decodeTags1 :: A.Value -> (T.Text, (T.Text, [T.Text]))
decodeTags1 (A.Array (toList -> (
	A.String k : A.String v : (((\(A.String o) -> o) <$>) -> os)))) =
	(k, (v, os))
decodeTags1 _ = error "bad"

fromScientific :: A.Value -> Int
fromScientific (A.Number s) = round s
fromScientific _ = error "bad"

encode :: T.Text -> Event.E -> IO A.Object
encode sec ev = do
	Just sec' <- pure $ Event.secretFromBech32 sec
	Just sig <- Event.signature sec' ev
	pure $ A.fromList [
		("content", A.String $ Event.content ev),
		("created_at",
			A.Number . fromIntegral . (\(CTime t) -> t) . toEpochTime $ Event.created_at ev),
		("id", A.String . T.pack . strToHexStr . BSC.unpack $ Event.hash ev),
		("kind", A.Number . fromIntegral $ Event.kind ev),
		("pubkey",
			A.String . T.pack . strToHexStr . tail
				. BSC.unpack . serialize_point . Event.pubkey $ ev),
		("sig", A.String . T.pack . strToHexStr $ BSC.unpack sig),
		("tags", tagsToJson $ Event.tags ev)
		]

tagsToJson :: Map.Map T.Text (T.Text, [T.Text]) -> A.Value
tagsToJson tgs = let
	tgs' = A.Array . V.fromList
			. ((\(k, (v, os)) ->
				A.Array $ V.fromList (A.String <$> (k : v : os))) <$>)
			$ Map.toList tgs in
	tgs'
