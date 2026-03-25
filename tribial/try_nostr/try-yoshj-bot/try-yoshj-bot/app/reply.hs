{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.Vector qualified as V
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson qualified as A
import Data.UnixTime
import System.Environment
import Network.WebSockets
import Crypto.Curve.Secp256k1
import Wuss

import Nostr.Event qualified as Event
import Nostr.Event.Signed qualified as Signed
import Nostr.Event.Json qualified as EvJsn
import Nostr.Filter
import Nostr.Filter.Json qualified as FlJsn

import "try-yoshj-bot" Tools

main :: IO ()
main = do
	scr : raddr : rprt : sc : pb : _ <- getArgs
	Right sk <- Event.secretFromBech32 <$> T.readFile sc
	Right pk <- Event.publicFromBech32 <$> T.readFile pb
	if (scr == "secure")
	then runSecureClient raddr (read rprt) "/" $ ws sk pk
	else runClient raddr (read rprt) "/" $ ws sk pk

ws :: Event.Secret -> Event.Pub -> ClientApp ()
ws sk pk cnn = do
	let	req0 = req "foobar12345" FlJsn.null
	sendTextData cnn req0
	forever do
		rdt <- receiveData cnn
		case A.decode rdt of
			Just (A.Array (V.toList -> [
				A.String "EOSE",
				A.String "foobar12345" ])) -> putStrLn "EOSE"
			Just (A.Array (V.toList -> [
				A.String "EVENT",
				A.String "foobar12345",
				A.Object jsn ])) -> do
					let	mev = EvJsn.decode' jsn
						mcnt = Signed.content <$> mev
						b = maybe False ("/yoshj-bot hello" `T.isPrefixOf`) mcnt
					Just ev' <- maybe (pure Nothing) ((Just <$>) . mkReply sk pk) mev
					print mev
					print ev'
					print mcnt
					sev <- Signed.signature sk ev'
					Just jsn <- pure $ EvJsn.encode' sev
					when b . sendTextData cnn . A.encode
						. A.Array $ V.fromList [A.String "EVENT", A.Object jsn]
			_ -> print rdt

mkReply :: Event.Secret -> Event.Pub -> Signed.E -> IO Event.E
mkReply sec pub e = do
	ut <- getUnixTime
	pure Event.E {
		Event.pubkey = pub, Event.created_at = ut,
		Event.kind = 1, Event.content = "Hello",
		Event.tags = [
			("p", (toHex . BS.tail . serialize_point $ Signed.pubkey e, [])),
			("e", (toHex $ Signed.idnt e, ["", "root"]))
			]
		}

req :: T.Text -> Filter -> BSLC.ByteString
req nm = A.encode . A.Array . V.fromList
	. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode
