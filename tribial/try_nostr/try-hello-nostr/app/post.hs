{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Prelude hiding (until)
import Data.Vector qualified as V
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson qualified as A
import Data.UnixTime
import System.Environment
import Network.WebSockets
import Wuss
import Nostr.Event qualified as Event
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json as EvJs

import "try-hello-nostr" Tools

main :: IO ()
main = do
	scr : adr : prt : pb : sc : msg : _ <- getArgs
	pub <- T.readFile pb; sec <- T.readFile sc
	if (scr == "secure") 
	then runSecureClient adr (read prt) "/" (ws sec pub $ T.pack msg)
	else runClient adr (read prt) "/" (ws sec pub $ T.pack msg)

ws :: T.Text -> T.Text -> T.Text -> ClientApp ()
ws sc pb msg cnn = do
	putStrLn "CONNECTED"
	idnt <- T.pack . strToHexStr . BSC.unpack <$> write sc pb cnn msg
	putStrLn "AFTER WRITE"
	doWhile do
		Just r <- A.decode <$> receiveData cnn
		print r
		pure case r of
			A.Array (V.toList -> [
					A.String "OK", A.String idnt',
					A.Bool True, A.String _ ])
				| idnt' == idnt -> False
			_ -> True
	putStrLn "BYE!"
	sendClose cnn ("Bye!" :: T.Text)

write :: T.Text -> T.Text -> Connection -> T.Text -> IO BS.ByteString
write sc pb cnn msg = do
	Right sk <- pure $ Event.secretFromBech32 sc
	Right pk <- pure $ Event.publicFromBech32 pb
	writeMessage cnn sk pk msg

writeMessage ::
	Connection -> Event.Secret -> Event.Pub -> T.Text -> IO BS.ByteString
writeMessage cnn sk pk msg = do
	ut <- getUnixTime
	ev <- Signed.signature sk Event.E {
		Event.pubkey = pk, Event.created_at = ut,
		Event.kind = 1, Event.tags = [], Event.content = msg }
	Just jsn <- pure $ EvJs.encode' ev
	sendTextData cnn . A.encode
		. A.Array $ V.fromList [A.String "EVENT", A.Object jsn]
	pure $ Signed.idnt ev
