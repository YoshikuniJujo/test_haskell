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
import Data.Aeson qualified as A
import Data.UnixTime
import System.Environment
import Network.WebSockets
import Wuss
import Nostr.Event qualified as Event
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json as EvJs

import TryBech32
import "try-hello-nostr" Tools

main :: IO ()
main = do
	scr : adr : prt : pb : sc : ((== "--send") -> fsnd) : msg : _ <- getArgs
	pub <- chomp <$> T.readFile pb; sec <- chomp <$> T.readFile sc
	if (scr == "secure") 
	then runSecureClient adr (read prt) "/" (ws sec pub fsnd msg)
	else runClient adr (read prt) "/" (ws sec pub fsnd msg)

ws :: T.Text -> T.Text -> Bool -> String -> ClientApp ()
ws sec pub_ fsnd msg cnn = do
	Just pub <- pure $ dataPart' "npub" pub_
	idnt <- T.pack . strToHexStr . BSC.unpack <$> write fsnd sec pub_ cnn msg
	when fsnd $ doWhile do

		rdt <- receiveData cnn
		BSLC.putStrLn rdt
		let	r' = A.decode rdt :: Maybe A.Value

		pure case r' of
			Just (A.Array (V.toList -> [A.String "OK", A.String idnt', A.Bool True, A.String _]))
				| idnt' == idnt -> False
			Just _ -> True
			Nothing -> error "bad"

	sendClose cnn ("Bye!" :: T.Text)

write :: Bool -> T.Text -> T.Text -> Connection -> String -> IO BS.ByteString
write fsnd sec pub_ cnn msg = do
	Just pk <- pure $ Event.publicFromBech32 pub_
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
