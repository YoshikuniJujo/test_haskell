{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Event (

	-- * EVENT

	E(..), sample,

	-- * SIGNATURE

	signature, Secret(..), secretFromBech32, hash, serialize,

	-- * VERIFY

	verify,

	-- * PUBLIC KEY

	parse_point

	) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.Word.Wider
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UnixTime
import Data.Aeson
import Data.Aeson.KeyMap qualified as A
import Numeric
import System.Entropy
import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.Curve.Secp256k1

import Bech32
import Tools

data E = E {
	pubkey :: Pub,
	created_at :: UnixTime,
	kind :: Int,
	tags :: Map.Map T.Text (T.Text, [T.Text]),
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
		tags = Map.empty,
		content = "Hello" }

chomp :: T.Text -> T.Text
chomp t = if T.last t == '\n' then T.init t else t

serializeEvent :: E -> String
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
	Just cnt' = content <$> Just ev in
	serialize pk crat' (fromJust $ kind <$> Just ev) tgs' cnt'

serialize :: T.Text -> Int -> Int -> Value -> T.Text -> String
serialize pk crat knd tgs cnt = "[0," ++
	show pk ++ "," ++ show crat ++ "," ++ show knd ++ "," ++
	serializeTags tgs ++ "," ++ "\"" ++ T.unpack (esc cnt) ++ "\"]"

esc :: T.Text -> T.Text
esc "" = ""
esc ('\n' T.:< ts) = "\\n" <> esc ts
esc ('"' T.:< ts) = "\\\"" <> esc ts
esc ('\\' T.:< ts) = "\\\\" <> esc ts
esc ('\r' T.:< ts) = "\\r" <> esc ts
esc ('\t' T.:< ts) = "\\t" <> esc ts
esc ('\b' T.:< ts) = "\\b" <> esc ts
esc ('\f' T.:< ts) = "\\f" <> esc ts
esc (c T.:< ts) = c T.:< esc ts

serializeTags :: Value -> String
serializeTags (Array a) = '[' :
	L.intercalate "," (serializeStrArray <$> toList a) ++
	"]"
serializeTags _ = error "bad"

serializeStrArray :: Value -> String
serializeStrArray (Array a) = '[' :
	L.intercalate "," ((\(String s) -> show s) <$> toList a) ++ "]"
serializeStrArray _ = error "bad"

signature :: Secret -> E -> IO (Maybe BS.ByteString)
signature (Secret sec) ev = sign_schnorr sec (hash ev) <$> getEntropy 32

secretFromBech32 :: T.Text -> Maybe Secret
secretFromBech32 sec = parseSecret =<< dataPart' "nsec" sec

parseSecret :: BS.ByteString -> Maybe Secret
parseSecret = (Secret <$>) . parse_int256

newtype Secret = Secret Wider deriving Show

hash :: E -> BS.ByteString
hash = SHA256.hash . BSU.fromString . serializeEvent

verify :: E -> BS.ByteString -> Bool
verify e = verify_schnorr (hash e) (pubkey e)
