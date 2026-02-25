{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Prelude hiding (until)
import Data.Vector qualified as V
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson qualified as A
import Wuss
import Network.WebSockets
import System.Environment

import Nostr.Event qualified as Event
import Nostr.Event.Json qualified as EvJsn
import TryBech32

import Nostr.Filter
import Nostr.Filter.Json qualified as FlJsn

import "try-hello-nostr" Tools

main :: IO ()
main = do
	opt : scr : raddr : rprt : acc : _ <- getArgs
	pub <- T.readFile acc
	if (scr == "secure") 
	then runSecureClient raddr (read rprt) "/" (ws (readOpt opt) pub)
	else runClient raddr (read rprt) "/" (ws (readOpt opt) pub)

ws :: Opt -> T.Text -> ClientApp ()
ws opt pub cnn = do
	let	req0 = req "foobar12345" <$> mkFilter opt pub
	either putStrLn ((>>) <$> BSLC.putStrLn <*> sendTextData cnn) req0
	putStrLn ""
	doWhile $ receiveData cnn >>= \rdt -> do
		BSLC.putStrLn rdt
		case A.decode rdt of
			Just (A.Array (V.toList -> [
					A.String "EOSE",
					A.String "foobar12345" ])) -> pure False
			Just (A.Array (V.toList -> [
					A.String "EVENT",
					A.String "foobar12345",
					A.Object jsn ])) -> do
				midnt <- pure $ fst
					<$> (lookup "e" =<< Event.tags <$> EvJsn.decode jsn)
				case midnt of
					Just idnt -> do
						print $ filterFromId idnt
						sendTextData cnn . req "barbaz54321" $ filterFromId idnt
					Nothing -> pure ()
				pure True
			Just (A.Array (V.toList -> [
					A.String "EVENT",
					A.String "barbaz54321",
					A.Object jsn ])) -> do
				putStrLn "barbaz54321"
				print jsn
				pure True
			Just _ -> pure True; Nothing -> error "bad"
	doWhile $ receiveData cnn >>= \rdt -> do
		BSLC.putStrLn rdt
		case A.decode rdt of
			Just (A.Array (V.toList -> [
					A.String "EOSE",
					A.String "barbaz54321" ])) -> pure False
			Just _ -> pure True
			Nothing -> error "bad"
	sendClose cnn ("Bye!" :: T.Text)

mkFilter :: Opt -> T.Text -> Either String Filter
mkFilter opt a = do
	pk <- Event.publicFromBech32 a
	pub <- maybe (Left "error") (Right . toHex) $ dataPart' "npub" (chomp a)
	case opt of
		Author -> pure Filter {
			ids = Nothing,
			authors = Just [pk], kinds = Just [1], tags = [],
			since = Nothing, until = Nothing, limit = Just 5 }
		P -> pure Filter {
			ids = Nothing,
			authors = Nothing, kinds = Just [1],
			tags = [('p', [pub])],
			since = Nothing, until = Nothing, limit = Just 5 }
		Others -> Left "no such option"

filterFromId :: T.Text -> Filter
filterFromId idnt = Filter {
	ids = Just [fromHex idnt],
	authors = Nothing, kinds = Nothing,
	tags = [], since = Nothing, until = Nothing, limit = Just 5 }

data Opt = Author | P | Others deriving Show

readOpt :: String -> Opt
readOpt = \case "--author" -> Author; "-p" -> P; _ -> Others

req :: T.Text -> Filter -> BSLC.ByteString
req nm = A.encode . A.Array . V.fromList
	. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode
