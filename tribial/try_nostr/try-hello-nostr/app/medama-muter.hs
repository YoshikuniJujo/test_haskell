{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.UTF8 qualified as BSU
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson
import Data.Aeson.KeyMap qualified as A
import Numeric
import Wuss
import Network.WebSockets
import Crypto.Hash.SHA256
import Crypto.Curve.Secp256k1

import Event

import System.Environment
import Data.UnixTime

main :: IO ()
main = do
	scr : raddr : rprt : _ <- getArgs
	if (scr == "secure")
		then runSecureClient raddr (read rprt) "/" ws
		else runClient raddr (read rprt) "/" ws

fltr :: Int -> String -> T.Text
fltr k a = "{ \"kinds\": [" <> T.pack (show k) <> "], " <>
	"\"authors\": [\"" <> T.pack a <> "\"] }"

ws :: ClientApp ()
ws cnn = do
	putStrLn "Connected!\n"

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

pubkeyToText :: Projective -> T.Text
pubkeyToText = T.pack . strToHexStr . tail . BSC.unpack . serialize_point

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

getEvent :: Value -> Maybe Event
getEvent = \case
	Array (toList -> [String "EOSE", String "foobar12345"]) -> Nothing
	Array (toList -> [
		String "EVENT", String "foobar12345", Object obj ]) ->
		jsonToEvent obj
	_ -> Nothing
