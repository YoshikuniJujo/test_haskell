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
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UnixTime
import Data.Aeson
import Wuss
import Network.WebSockets
import System.Environment

import Event
import TryBech32
import Tools

main :: IO ()
main = do
	scr : raddr : rprt : acc : foo : ((== "--send") -> fsnd) : msg : _ <-
		getArgs
	Just pub <- dataPart . T.init <$> T.readFile acc
	sec <- T.init <$> T.readFile foo
	if (scr == "secure") 
	then runSecureClient raddr (read rprt) "/" (ws sec pub fsnd msg)
	else runClient raddr (read rprt) "/" (ws sec pub fsnd msg)

ws :: T.Text -> BS.ByteString -> Bool -> String -> ClientApp ()
ws sec pub fsnd msg cnn = do
	putStrLn "Connected!\n"

	let	pubTxt = T.pack . strToHexStr $ BSC.unpack pub
	sendTextData cnn $ "[\"REQ\", \"foobar12345\", " <> fltr pubTxt <> "]"

	Just r <- decode <$> receiveData cnn
	case r of
		Array (toList -> [String "EOSE", String "foobar12345"]) -> do
			putStrLn "EOSE RECEIVED"
		Array (toList -> [
			String "EVENT", String "foobar12345", Object obj ]) -> do
			let	ev = jsonToEvent obj
			print ev
			putStrLn ""
			maybe (pure ()) (T.putStrLn . content) ev
		_ -> error "bad"

	putStrLn "\n*** EVENT ***"

	Just pk <- pure $ parse_point pub
	ut <- getUnixTime
	json <- eventToJson sec Event {
		pubkey = pk,
		created_at = ut,
		kind = 1,
		tags = Map.empty,
		content = T.pack msg
		}

	print $ jsonToEvent json

	when fsnd . sendTextData cnn
		. encode . Array $ V.fromList [String "EVENT", Object json]
	Just r' <- decode <$> receiveData cnn :: IO (Maybe Value)
	print r'

	sendClose cnn ("Bye!" :: T.Text)
	where fltr a = "{ \"kinds\": [1], " <>
		"\"authors\": [\"" <> a <> "\"] }"
