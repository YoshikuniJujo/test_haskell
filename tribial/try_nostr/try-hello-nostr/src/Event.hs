{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event (

	Event(..), jsonToEvent, eventToJson,

	parse_point

	) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.UnixTime
import Data.Aeson
import Data.Aeson.KeyMap qualified as A
import Numeric
import System.Entropy
import Crypto.Hash.SHA256
import Crypto.Curve.Secp256k1

import TryBech32
import Tools

data Event = Event {
	pubkey :: Pub,
	created_at :: UnixTime,
	kind :: Int,
	tags :: Map.Map T.Text (T.Text, [T.Text]),
	content :: T.Text }
	deriving Show

serializeEvent :: Event -> String
serializeEvent ev = let
	pk = T.pack . strToHexStr . tail . BSC.unpack . serialize_point . pubkey $ ev
	Just crat' = fromIntegral
			. (\(CTime t) -> t)
			. toEpochTime . created_at <$> Just ev
	Just tgs' = Array . V.fromList
			. ((\(k, (v, os)) ->
				Array $ V.fromList (String <$> (k : v : os))) <$>)
			. Map.toList . tags
			<$> Just ev
	Just cnt' = esc . content <$> Just ev in
	serialize pk crat' (fromJust $ kind <$> Just ev) tgs' cnt'

serialize :: T.Text -> Int -> Int -> Value -> T.Text -> String
serialize pk crat knd tgs cnt = "[0," ++
	show pk ++ "," ++ show crat ++ "," ++ show knd ++ "," ++
	serializeTags tgs ++ "," ++ "\"" ++ T.unpack cnt ++ "\"]"

serializeTags :: Value -> String
serializeTags (Array a) = '[' :
	L.intercalate "," (serializeStrArray <$> toList a) ++
	"]"
serializeTags _ = error "bad"

serializeStrArray :: Value -> String
serializeStrArray (Array a) = '[' :
	L.intercalate "," ((\(String s) -> show s) <$> toList a) ++ "]"
serializeStrArray _ = error "bad"

esc "" = ""
esc ('\n' T.:< ts) = "\\n" <> esc ts
esc ('"' T.:< ts) = "\\\"" <> esc ts
esc ('\\' T.:< ts) = "\\\\" <> esc ts
esc ('\r' T.:< ts) = "\\r" <> esc ts
esc ('\t' T.:< ts) = "\\t" <> esc ts
esc ('\b' T.:< ts) = "\\b" <> esc ts
esc ('\f' T.:< ts) = "\\f" <> esc ts
esc (c T.:< ts) = c T.:< esc ts

jsonToEvent :: Object -> Maybe Event
jsonToEvent obj = do
	String pk <- A.lookup "pubkey" obj
	crat <- fromScientific <$> A.lookup "created_at" obj
	knd <- fromScientific <$> A.lookup "kind" obj
	tgs <- A.lookup "tags" obj
	cnt <- (\(String s) -> s) <$> A.lookup "content" obj
	String sig <- A.lookup "sig" obj
	let	srzd = serialize pk crat knd tgs (esc cnt)
		hshd = hash $ BSU.fromString srzd
	pk' <- parse_point . BS.pack . (fst . head . readHex <$>) . separate 2 $ T.unpack pk
	let	sig' = BS.pack . (fst . head . readHex <$>) . separate 2 $ T.unpack sig
	guard $ verify_schnorr hshd pk' sig'
	pure Event {
		pubkey = pk',
		created_at = fromEpochTime . CTime $ fromIntegral crat,
		kind = knd,
		tags = decodeTags tgs,
		content = cnt }

decodeTags :: Value -> Map.Map T.Text (T.Text, [T.Text])
decodeTags (Array (toList -> ts)) = Map.fromList $ decodeTags1 <$> ts
decodeTags _ = error "bad"

decodeTags1 :: Value -> (T.Text, (T.Text, [T.Text]))
decodeTags1 (Array (toList -> (
	String k : String v : (((\(String o) -> o) <$>) -> os)))) =
	(k, (v, os))
decodeTags1 _ = error "bad"

fromScientific :: Value -> Int
fromScientific (Number s) = round s
fromScientific _ = error "bad"

eventToJson :: T.Text -> Event -> IO Object
eventToJson sec ev = do
	sig <- signatureEvent sec ev
	pure $ A.fromList [
		("content", String $ content ev),
		("created_at",
			Number . fromIntegral . (\(CTime t) -> t) . toEpochTime $ created_at ev),
		("id", String . T.pack . strToHexStr . BSC.unpack $ hashEvent ev),
		("kind", Number . fromIntegral $ kind ev),
		("pubkey",
			String . T.pack . strToHexStr . tail
				. BSC.unpack . serialize_point . pubkey $ ev),
		("sig", String . T.pack . strToHexStr $ BSC.unpack sig),
		("tags", tagsToJson $ tags ev)
		]

tagsToJson :: Map.Map T.Text (T.Text, [T.Text]) -> Value
tagsToJson tgs = let
	tgs' = Array . V.fromList
			. ((\(k, (v, os)) ->
				Array $ V.fromList (String <$> (k : v : os))) <$>)
			$ Map.toList tgs in
	tgs'

hashEvent :: Event -> BS.ByteString
hashEvent = hash . BSU.fromString . serializeEvent

signatureEvent :: T.Text -> Event -> IO BS.ByteString
signatureEvent sec ev = do
	Just sec' <- pure $ parse_int256 =<< dataPart sec
	aux <- getEntropy 32
	Just sig'' <- pure $ sign_schnorr sec' (hashEvent ev) aux
	pure sig''
