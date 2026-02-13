{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Event.Json (

	-- * CODEC BETWEEN EVENT AND JSON

	encode, decode,

	-- * CODEC BEGWEEN SIGNED EVENT AND JSON

	encode', decode',

	-- * CODEC TAGS

	encodeTags, decodeTags

	) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Vector qualified as V
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
	cnt <- getString <$> A.lookup "content" obj
	A.String sig <- A.lookup "sig" obj
	pk' <- parse_point . BS.pack
		. (fst . head . readHex <$>) . separate 2 $ T.unpack pk
	let	idnt' = BS.pack . (fst . head . readHex <$>)
			. separate 2 $ T.unpack idnt
		sig' = BS.pack . (fst . head . readHex <$>)
			. separate 2 $ T.unpack sig
		srzd = Event.serialize pk crat knd tgs cnt
		hshd = hash $ BSU.fromString srzd
	guard $ verify_schnorr hshd pk' sig'
	pure Signed.E {
		Signed.idnt = idnt',
		Signed.pubkey = pk',
		Signed.created_at = fromEpochTime . CTime $ fromIntegral crat,
		Signed.kind = knd,
		Signed.tags = decodeTags tgs,
		Signed.content = cnt,
		Signed.sig = sig',
		Signed.verified = True }

fromScientific :: A.Value -> Int
fromScientific (A.Number s) = round s
fromScientific _ = error "bad"

decodeTags :: A.Value -> [(T.Text, (T.Text, [T.Text]))]
decodeTags (A.Array (toList -> ts)) = decodeTags1 <$> ts
decodeTags _ = error "bad"

decodeTags1 :: A.Value -> (T.Text, (T.Text, [T.Text]))
decodeTags1 (A.Array (toList -> kv)) = case kv of
	A.String k : A.String v : ((getString <$>) -> os) -> (k, (v, os))
	_ -> error "bad"
decodeTags1 _ = error "bad"

getString :: A.Value -> T.Text
getString (A.String s) = s
getString _ = error "bad"

encode :: Event.Secret -> Event.E -> IO A.Object
encode sec ev = do
	Just o <- encode' <$> Signed.signature sec ev
	pure o

encode' :: Signed.E -> Maybe A.Object
encode' ev = do
	let	sig = Signed.sig ev
	guard $ Signed.verified ev
	pure $ A.fromList [
		("content", A.String $ Signed.content ev),
		("created_at", A.Number . fromIntegral . (\(CTime t) -> t)
			. toEpochTime $ Signed.created_at ev),
		("id", A.String
			. T.pack . strToHexStr . BSC.unpack $ Signed.idnt ev),
		("kind", A.Number . fromIntegral $ Signed.kind ev),
		("pubkey", A.String . T.pack . strToHexStr . tail
			. BSC.unpack . serialize_point $ Signed.pubkey ev),
		("sig", A.String . T.pack . strToHexStr $ BSC.unpack sig),
		("tags", encodeTags $ Signed.tags ev) ]

encodeTags :: [(T.Text, (T.Text, [T.Text]))] -> A.Value
encodeTags = A.Array . V.fromList .
	((\(k, (v, os)) -> A.Array $ V.fromList (A.String <$> (k : v : os)))
		<$>)
