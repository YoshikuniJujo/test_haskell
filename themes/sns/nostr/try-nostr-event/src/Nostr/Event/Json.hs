{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Event.Json (

	-- * CODEC BETWEEN EVENT AND JSON

	encode, decode,

	-- * CODEC BETWEEN SIGNED EVENT AND JSON

	encode', decode',

	-- * CODEC TAGS

	encodeTags, decodeTags

	) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.UnixTime
import Crypto.Hash.SHA256
import Crypto.Curve.Secp256k1
import Numeric

import Nostr.Event qualified as Event
import Nostr.Event.Signed qualified as Signed
import Tools

decode :: A.Object -> Maybe Event.E
decode = Signed.verify <=< decode'

decode' :: A.Object -> Maybe Signed.E
decode' obj = do
	A.String idnt <- A.lookup "id" obj
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
	pure Signed.E {
		Signed.id = T.encodeUtf8 idnt,
		Signed.pubkey = pk',
		Signed.created_at = fromEpochTime . CTime $ fromIntegral crat,
		Signed.kind = knd,
		Signed.tags = decodeTags tgs,
		Signed.content = cnt,
		Signed.sig = T.encodeUtf8 sig,
		Signed.verified = True }

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

encode :: Event.Secret -> Event.E -> IO A.Object
encode sec ev = do
	Just o <- encode' <$> Signed.signature sec ev
	pure o

encode' :: Signed.E -> Maybe A.Object
encode' ev = let sig = Signed.sig ev in if Signed.verified ev
	then Just $ A.fromList [
		("content", A.String $ Signed.content ev),
		("created_at",
			A.Number . fromIntegral . (\(CTime t) -> t) . toEpochTime $ Signed.created_at ev),
		("id", A.String . T.pack . strToHexStr . BSC.unpack $ Signed.id ev),
		("kind", A.Number . fromIntegral $ Signed.kind ev),
		("pubkey",
			A.String . T.pack . strToHexStr . tail
				. BSC.unpack . serialize_point . Signed.pubkey $ ev),
		("sig", A.String . T.pack . strToHexStr $ BSC.unpack sig),
		("tags", encodeTags $ Signed.tags ev) ]
	else Nothing

encodeTags :: Map.Map T.Text (T.Text, [T.Text]) -> A.Value
encodeTags tgs = let
	tgs' = A.Array . V.fromList
		. ((\(k, (v, os)) ->
			A.Array $ V.fromList (A.String <$> (k : v : os))) <$>)
		$ Map.toList tgs in
	tgs'
