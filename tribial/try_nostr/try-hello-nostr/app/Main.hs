{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson
import Data.Aeson.KeyMap qualified as A
import System.Entropy
import Numeric
import Wuss
import Network.WebSockets
import Crypto.Hash.SHA256
import Crypto.Curve.Secp256k1

import TryBech32
import Event

import System.Environment
import Data.UnixTime

main :: IO ()
main = do
	acc : foo : _ <- getArgs
	pub <- L.init <$> readFile acc
	sec <- L.init <$> readFile foo
	runSecureClient "nos.lol" 443 "/" (ws pub sec)

fltr :: String -> T.Text
fltr a = "{ \"kinds\": [1], " <>
	"\"authors\": [\"" <> T.pack a <> "\"] }"

ws :: String -> String -> ClientApp ()
ws pub sec cnn = do
	let	Just pubStr = strToHexStr . BSC.unpack <$> dataPart (T.pack pub)
		Just sec' = parse_int256 =<< dataPart (T.pack sec)
	putStrLn "Connected!"

	sendTextData cnn
		("[\"REQ\", \"foobar12345\", " <> fltr pubStr <> "]" :: T.Text)

	Just (Array (toList -> [
		String "EVENT", String "foobar12345", Object obj ])) <-
		decode <$> receiveData cnn :: IO (Maybe Value)

	let	Just (String pk) = A.lookup "pubkey" obj
	guard $ pk == T.pack pubStr
	let	Just hshd = do
			crat <- fromScientific <$> A.lookup "created_at" obj
			knd <- fromScientific <$> A.lookup "kind" obj
			tgs <- A.lookup "tags" obj
			cnt <- escape <$> A.lookup "content" obj
			let	srzd = serialize pk crat knd tgs cnt
			pure . hash $ BSU.fromString srzd
		Just pk' = parse_point . BS.pack . (fst . head . readHex <$>) . separate 2 $ pubStr

	aux <- getEntropy 32
	let	Just sig'' = sign_schnorr sec' hshd aux
	print $ verify_schnorr hshd pk' sig''

	let	ev = jsonToEvent obj
	print ev
	putStrLn ""
	maybe (pure ()) (T.putStrLn . content) ev

	sendClose cnn ("Bye!" :: T.Text)

jsonToEvent :: Object -> Maybe Event
jsonToEvent obj = do
	String pk <- A.lookup "pubkey" obj
	crat <- fromScientific <$> A.lookup "created_at" obj
	knd <- fromScientific <$> A.lookup "kind" obj
	tgs <- A.lookup "tags" obj
	cnt <- escape <$> A.lookup "content" obj
	String sig <- A.lookup "sig" obj
	let	srzd = serialize pk crat knd tgs cnt
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

escape :: Value -> T.Text
escape (String txt) = esc txt
	where
	esc "" = ""
	esc ('\n' T.:< ts) = "\\n" <> esc ts
	esc ('"' T.:< ts) = "\\\"" <> esc ts
	esc ('\\' T.:< ts) = "\\\\" <> esc ts
	esc ('\r' T.:< ts) = "\\r" <> esc ts
	esc ('\t' T.:< ts) = "\\t" <> esc ts
	esc ('\b' T.:< ts) = "\\b" <> esc ts
	esc ('\f' T.:< ts) = "\\f" <> esc ts
	esc (c T.:< ts) = c T.:< esc ts
escape _ = error "bad"

fromScientific :: Value -> Int
fromScientific (Number s) = round s
fromScientific _ = error "bad"

serialize :: T.Text -> Int -> Int -> Value -> T.Text -> String
serialize pk crat knd tgs cnt = "[0," ++
	show pk ++ "," ++ show crat ++ "," ++ show knd ++ "," ++
	serializeTags tgs ++ "," ++ "\"" ++ T.unpack cnt ++ "\"]"

serializeTags :: Value -> String
serializeTags (Array a) = '[' :
	L.intercalate "," (serializeStrArray <$> toList a) ++
	"]"
serializeTags _ = error "bad"

decodeTags :: Value -> Map.Map T.Text (T.Text, [T.Text])
decodeTags (Array (toList -> ts)) = Map.fromList $ decodeTags1 <$> ts
decodeTags _ = error "bad"

decodeTags1 :: Value -> (T.Text, (T.Text, [T.Text]))
decodeTags1 (Array (toList -> (
	String k : String v : (((\(String o) -> o) <$>) -> os)))) =
	(k, (v, os))
decodeTags1 _ = error "bad"

serializeStrArray :: Value -> String
serializeStrArray (Array a) = '[' :
	L.intercalate "," ((\(String s) -> show s) <$> toList a) ++ "]"
serializeStrArray _ = error "bad"

strToHexStr :: String -> String
strToHexStr = concat . (sh <$>) . map ord
	where
	sh n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

separate :: Int -> String -> [String]
separate _ "" = []
separate n s = take n s : separate n (drop n s)
