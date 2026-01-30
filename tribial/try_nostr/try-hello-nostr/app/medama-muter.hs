{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.Char
import Data.Word.Wider
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
	scr : raddr : rprt : acc : foo : snd : msg : _ <- getArgs
	pub <- L.init <$> readFile acc
	sec <- L.init <$> readFile foo
	if (scr == "secure")
		then runSecureClient raddr (read rprt) "/" (ws (snd == "--send") msg pub sec)
		else runClient raddr (read rprt) "/" (ws (snd == "--send") msg pub sec)

fltr :: Int -> String -> T.Text
fltr k a = "{ \"kinds\": [" <> T.pack (show k) <> "], " <>
	"\"authors\": [\"" <> T.pack a <> "\"] }"

ws :: Bool -> String -> String -> String -> ClientApp ()
ws snd msg pub sec cnn = do
	putStrLn "Connected!\n"

	let	Just pubStr = strToHexStr . BSC.unpack <$> dataPart (T.pack pub)
	sendTextData cnn
--		("[\"REQ\", \"foobar12345\", " <> fltr pubStr <> "]" :: T.Text)
		("[\"REQ\", \"foobar12345\", { \"kinds\": [7]}]" :: T.Text)

	evs <- doWhile 10000 do
		dt <- receiveData cnn
		let	ev = getEvent =<< decode dt
		pure ev
	print evs
	print $ content <$> evs
	let notMedama = head $ pubkeyToText . pubkey <$> evs
	print notMedama
--	let medamas = pubkeyToText . pubkey <$> filter ((== "\128065\65039") . content) evs
	let medamas = pubkeyToText . pubkey <$> filter ((== "\128065") . content) evs
--	let medamas = pubkeyToText . pubkey <$> filter ((== '\128065') . T.head . content) evs
	print medamas

	sendTextData cnn ("[\"CLOSE\", \"foobar12345\"]" :: T.Text)

{-
	sendTextData cnn
		("[\"REQ\", \"barbaz12345\", " <> fltr 0 (T.unpack notMedama) <> "]" :: T.Text)
	print @T.Text =<< receiveData cnn
	sendTextData cnn ("[\"CLOSE\", \"barbaz12345\"]" :: T.Text)
	-}
	print @T.Text =<< receiveData cnn

	sendTextData cnn
		("[\"REQ\", \"medama\", " <> fltr 0 (T.unpack $ head medamas) <> "]" :: T.Text)

	print @T.Text =<< receiveData cnn
--	print @T.Text =<< receiveData cnn

--	print @T.Text =<< receiveData cnn
--	print @T.Text =<< receiveData cnn
--	T.putStrLn =<< receiveData cnn
--	T.putStrLn =<< receiveData cnn
--	print @T.Text =<< receiveData cnn
	T.putStrLn =<< receiveData cnn
	T.putStrLn =<< receiveData cnn
	T.putStrLn =<< receiveData cnn
	T.putStrLn =<< receiveData cnn
	T.putStrLn =<< receiveData cnn
	T.putStrLn =<< receiveData cnn
	T.putStrLn =<< receiveData cnn

	sendClose cnn ("Bye!" :: T.Text)

doWhile :: Monad m => Int -> m (Maybe a) -> m [a]
doWhile mx _ | mx < 1 = pure []
doWhile mx act = act >>= \case
	Nothing -> pure []
	Just x -> (x :) <$> doWhile (mx - 1) act

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

pubkeyToText = T.pack . strToHexStr . tail
	. BSC.unpack . serialize_point

escape :: Value -> T.Text
escape (String txt) = esc txt
escape _ = error "bad"

esc "" = ""
esc ('\n' T.:< ts) = "\\n" <> esc ts
esc ('"' T.:< ts) = "\\\"" <> esc ts
esc ('\\' T.:< ts) = "\\\\" <> esc ts
esc ('\r' T.:< ts) = "\\r" <> esc ts
esc ('\t' T.:< ts) = "\\t" <> esc ts
esc ('\b' T.:< ts) = "\\b" <> esc ts
esc ('\f' T.:< ts) = "\\f" <> esc ts
esc (c T.:< ts) = c T.:< esc ts

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
	let	Just sec' = parse_int256 =<< dataPart (T.pack sec)
	aux <- getEntropy 32
	let	Just sig'' = sign_schnorr sec' (hashEvent ev) aux
	pure sig''

getContent = \case
	Array (toList -> [String "EOSE", String "foobar12345"]) -> Nothing
	Array (toList -> [
		String "EVENT", String "foobar12345", Object obj ]) ->
		content <$> jsonToEvent obj
	_ -> Nothing

getEvent = \case
	Array (toList -> [String "EOSE", String "foobar12345"]) -> Nothing
	Array (toList -> [
		String "EVENT", String "foobar12345", Object obj ]) ->
		jsonToEvent obj
	_ -> Nothing
