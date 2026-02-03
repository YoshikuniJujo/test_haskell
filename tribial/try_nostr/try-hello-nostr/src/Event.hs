{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event where

import Foreign.C.Types
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.Char
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Data.UnixTime
import Data.Aeson
import Numeric
import Crypto.Curve.Secp256k1

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

strToHexStr :: String -> String
strToHexStr = concat . (sh <$>) . map ord
	where
	sh n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

esc "" = ""
esc ('\n' T.:< ts) = "\\n" <> esc ts
esc ('"' T.:< ts) = "\\\"" <> esc ts
esc ('\\' T.:< ts) = "\\\\" <> esc ts
esc ('\r' T.:< ts) = "\\r" <> esc ts
esc ('\t' T.:< ts) = "\\t" <> esc ts
esc ('\b' T.:< ts) = "\\b" <> esc ts
esc ('\f' T.:< ts) = "\\f" <> esc ts
esc (c T.:< ts) = c T.:< esc ts
