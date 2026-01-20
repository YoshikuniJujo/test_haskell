{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Foldable
import Data.List qualified as L
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.Scientific
import Data.Aeson
import Data.Aeson.KeyMap qualified as A
import System.Entropy
import Numeric
import Wuss
import Network.WebSockets
import Crypto.Hash.SHA256
import Crypto.Curve.Secp256k1

import TryBech32

import System.Environment

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

	sendTextData cnn (
		"[\"REQ\", \"foobar12345\", " <> fltr pubStr <> "]"
			:: T.Text )

	(Just (Array (toList -> [_, _, Object obj])) :: Maybe Value) <- decode <$> receiveData cnn

	print obj

	putStrLn "\n*** FOR SERIALIZE ***\n"

	let	Just (String pk) = A.lookup "pubkey" obj
		Just crat = fromScientific <$> A.lookup "created_at" obj
		Just knd = fromScientific <$> A.lookup "kind" obj
		Just tgs = A.lookup "tags" obj
		Just idnt = A.lookup "id" obj
--		Just cnt = escape <$> A.lookup "content" obj
		Just cnt = escape <$> A.lookup "content" obj
		Just (String sig) = A.lookup "sig" obj

	print pk
	print crat
	print knd
	print tgs
	print cnt

	putStrLn ""
	let	srzd = serialize pk crat knd tgs cnt
	putStrLn srzd
	putStrLn . strToHexStr . BSC.unpack . hash $ BSU.fromString srzd
	print idnt

	putStrLn "\n*** FOR SIGNATURE ***\n"

	let	hshd = hash $ BSU.fromString srzd
		Just pk' = parse_point . BS.pack . (fst . head . readHex <$>) . separate 2 $ T.unpack pk
		sig' = BS.pack . (fst . head . readHex <$>) . separate 2 $ T.unpack sig

	print hshd
	print pk'
	print sig'

	print $ verify_schnorr hshd pk' sig'

	aux <- getEntropy 32
	let	Just sig'' = sign_schnorr sec' hshd aux

	print sig''
	print $ verify_schnorr hshd pk' sig''

	putStrLn "\n*** PUB KEY ***\n"

	print pubStr
	print sec'

	sendClose cnn ("Bye!" :: T.Text)

escape, escape' :: Value -> T.Text
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

escape' (String txt) = txt

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
