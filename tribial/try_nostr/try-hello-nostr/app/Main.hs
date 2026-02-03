{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Map qualified as Map
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
	scr : raddr : rprt : acc : foo : fsnd : msg : _ <- getArgs
	pub <- L.init <$> readFile acc
	sec <- L.init <$> readFile foo
	if (scr == "secure")
		then runSecureClient raddr (read rprt) "/" (ws (fsnd == "--send") msg pub sec)
		else runClient raddr (read rprt) "/" (ws (fsnd == "--send") msg pub sec)

fltr :: String -> T.Text
fltr a = "{ \"kinds\": [1], " <>
	"\"authors\": [\"" <> T.pack a <> "\"] }"

ws :: Bool -> String -> String -> String -> ClientApp ()
ws fsnd msg pub sec cnn = do
	putStrLn "Connected!\n"

	Just pubStr <- pure $ strToHexStr . BSC.unpack <$> dataPart (T.pack pub)
	sendTextData cnn
		("[\"REQ\", \"foobar12345\", " <> fltr pubStr <> "]" :: T.Text)

	dt <- receiveData cnn
	print dt
	Just r <- pure $ decode dt
	case r of
		Array (toList -> [String "EOSE", String "foobar12345"]) -> do
			putStrLn "EOSE RECEIVED"
		Array (toList -> [
			String "EVENT", String "foobar12345", Object obj ]) -> do
			print obj
			let	ev = jsonToEvent obj
			print ev
			putStrLn ""
			maybe (pure ()) (T.putStrLn . content) ev

			Just (String pk) <- pure $ A.lookup "pubkey" obj
			guard $ pk == T.pack pubStr

			putStrLn "\n*** FOR SERIALIZE FROM EVENT ***"

			maybe (pure ()) (putStrLn . serializeEvent) ev

			putStrLn "\n*** SIGNATURE ***"

			Just sig'' <- maybe (pure Nothing) ((Just <$>) . signatureEvent sec) ev

			Just hshd' <- pure $ hash . BSU.fromString . serializeEvent <$> ev
			print $ verify_schnorr hshd' (fromJust $ pubkey <$> ev) sig''

			putStrLn "\n*** MAKE EVENT ***"

			print obj

			Just ev' <- pure ev
			print $ pubkey ev'
		_ -> error "bad"
	Just pk' <- pure $ parse_point . BS.pack . (fst . head . readHex <$>) $ separate 2 pubStr
	print pk'
	ut <- getUnixTime
	json <- eventToJson sec Event {
		pubkey = pk',
		created_at = ut,
		kind = 1,
		tags = Map.empty,
		content = T.pack msg
		}

	print json

	print $ jsonToEvent json

	putStrLn "\n*** EVENT ***"

	let
		eventToSend = encode . Array $ V.fromList [String "EVENT", Object json]

	print eventToSend

	when fsnd $ sendTextData cnn eventToSend
	Just r' <- decode <$> receiveData cnn :: IO (Maybe Value)
	print r'

	sendClose cnn ("Bye!" :: T.Text)

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

eventToJson :: String -> Event -> IO Object
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

fromScientific :: Value -> Int
fromScientific (Number s) = round s
fromScientific _ = error "bad"

decodeTags :: Value -> Map.Map T.Text (T.Text, [T.Text])
decodeTags (Array (toList -> ts)) = Map.fromList $ decodeTags1 <$> ts
decodeTags _ = error "bad"

decodeTags1 :: Value -> (T.Text, (T.Text, [T.Text]))
decodeTags1 (Array (toList -> (
	String k : String v : (((\(String o) -> o) <$>) -> os)))) =
	(k, (v, os))
decodeTags1 _ = error "bad"

separate :: Int -> String -> [String]
separate _ "" = []
separate n s = take n s : separate n (drop n s)

tagsToJson :: Map.Map T.Text (T.Text, [T.Text]) -> Value
tagsToJson tgs = let
	tgs' = Array . V.fromList
			. ((\(k, (v, os)) ->
				Array $ V.fromList (String <$> (k : v : os))) <$>)
			$ Map.toList tgs in
	tgs'

hashEvent :: Event -> BS.ByteString
hashEvent = hash . BSU.fromString . serializeEvent

signatureEvent :: String -> Event -> IO BS.ByteString
signatureEvent sec ev = do
	Just sec' <- pure $ parse_int256 =<< dataPart (T.pack sec)
	aux <- getEntropy 32
	Just sig'' <- pure $ sign_schnorr sec' (hashEvent ev) aux
	pure sig''
