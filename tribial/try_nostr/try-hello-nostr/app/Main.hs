{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Control.Monad
import Data.Foldable
import Data.Vector qualified as V
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UnixTime
import Data.Aeson qualified as A
import Wuss
import Network.WebSockets
import System.Environment

import Nostr.Event qualified as Event
import Nostr.Event.Json as EvJs
import TryBech32
import Tools

main :: IO ()
main = do
	scr : raddr : rprt : acc : foo : ((== "--send") -> fsnd) : msg : _ <-
		getArgs
	Just pub <- dataPart . chomp <$> T.readFile acc
	sec <- chomp <$> T.readFile foo
	if (scr == "secure") 
	then runSecureClient raddr (read rprt) "/" (ws sec pub fsnd msg)
	else runClient raddr (read rprt) "/" (ws sec pub fsnd msg)

chomp :: T.Text -> T.Text
chomp t = if T.last t == '\n' then T.init t else t

ws :: T.Text -> BS.ByteString -> Bool -> String -> ClientApp ()
ws sec pub fsnd msg cnn = do
	putStrLn "Connected!\n"

	let	pubTxt = T.pack . strToHexStr $ BSC.unpack pub
	sendTextData cnn $ "[\"REQ\", \"foobar12345\", " <> fltr pubTxt <> "]"
	print $ "[\"REQ\", \"foobar12345\", " <> fltr pubTxt <> "]"

	Just r <- A.decode <$> receiveData cnn
	print r
	case r of
		A.Array (toList -> [A.String "EOSE", A.String "foobar12345"]) -> do
			putStrLn "EOSE RECEIVED"
		A.Array (toList -> [
			A.String "EVENT", A.String "foobar12345", A.Object obj ]) -> do
			let	ev = EvJs.decode obj
			print ev
			putStrLn ""
			maybe (pure ()) (T.putStrLn . Event.content) ev
		_ -> error "bad"

	putStrLn "\n*** WRITE ***"

	when fsnd $ write sec pub cnn msg

	rdt <- receiveData cnn
	BSLC.putStrLn rdt
	Just r' <- pure $ A.decode rdt :: IO (Maybe A.Value) -- <$> receiveData cnn :: IO (Maybe A.Value)
	print r'

	sendClose cnn ("Bye!" :: T.Text)
	where fltr a = "{ \"kinds\": [1], " <>
		"\"authors\": [\"" <> a <> "\"] }"

write :: T.Text -> BSC.ByteString -> Connection -> String -> IO ()
write sec pub cnn msg = do
	Just pk <- pure $ Event.parse_point pub
	ut <- getUnixTime
	Just sec' <- pure $ Event.secretFromBech32 sec
	json <- EvJs.encode sec' Event.E {
		Event.pubkey = pk, Event.created_at = ut,
		Event.kind = 1, Event.tags = Map.empty, Event.content = T.pack msg }
	print json
	sendTextData cnn
		. A.encode . A.Array $ V.fromList [A.String "EVENT", A.Object json]
