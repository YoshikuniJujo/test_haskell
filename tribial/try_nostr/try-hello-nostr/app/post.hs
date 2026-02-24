{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Prelude hiding (until)
import Control.Monad
import Data.Vector qualified as V
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
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json as EvJs
import TryBech32

import "try-hello-nostr" Tools

main :: IO ()
main = do
	scr : raddr : rprt : acc : foo : ((== "--send") -> fsnd) : msg : _ <-
		getArgs
	pub_ <- T.readFile acc
	sec <- chomp <$> T.readFile foo
	if (scr == "secure") 
	then runSecureClient raddr (read rprt) "/" (ws sec pub_ fsnd msg)
	else runClient raddr (read rprt) "/" (ws sec pub_ fsnd msg)

ws :: T.Text -> T.Text -> Bool -> String -> ClientApp ()
ws sec pub_ fsnd msg cnn = do
	putStrLn "Connected!\n"

	Just pub <- pure . dataPart $ chomp pub_

	idnt <- T.pack . strToHexStr . BSC.unpack <$> write fsnd sec pub cnn msg

	when fsnd $ doWhile do

		rdt <- receiveData cnn
		BSLC.putStrLn rdt
		let	r' = A.decode rdt :: Maybe A.Value -- <$> receiveData cnn :: IO (Maybe A.Value)

		pure case r' of
			Just (A.Array (V.toList -> [A.String "OK", A.String idnt', A.Bool True, A.String _]))
				| idnt' == idnt -> False
	--		Just (A.Array (V.toList -> [A.String "EOSE", A.String "foobar12345"])) -> False
			Just _ -> True
			Nothing -> error "bad"

	putStrLn "BYE!"
	sendClose cnn ("Bye!" :: T.Text)

write :: Bool -> T.Text -> BSC.ByteString -> Connection -> String -> IO BS.ByteString
write fsnd sec pub cnn msg = do
	Just pk <- pure $ Event.parse_point pub
	ut <- getUnixTime
	Just sec' <- pure $ Event.secretFromBech32 sec
	let	ev = Event.E {
			Event.pubkey = pk, Event.created_at = ut,
			Event.kind = 1, Event.tags = [], Event.content = T.pack msg }
	ev' <- Signed.signature sec' ev
	Just json <- pure $ EvJs.encode' ev'
	let	event = A.encode . A.Array $ V.fromList [A.String "EVENT", A.Object json]
	when fsnd $ sendTextData cnn event
	pure $ Signed.idnt ev'
