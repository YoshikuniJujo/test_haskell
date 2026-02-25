{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Prelude hiding (until)
import Control.Monad
import Data.Foldable
import Data.Vector qualified as V
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

import Nostr.Filter
import Nostr.Filter.Json qualified as FlJsn

import "try-hello-nostr" Tools

data Opt = Author | P | Others
	deriving Show

readOpt :: String -> Opt
readOpt "--author" = Author
readOpt "-p" = P
readOpt _ = Others

main :: IO ()
main = do
	opt : scr : raddr : rprt : acc : foo : ((== "--send") -> fsnd) : msg : acc' : _ <-
		getArgs
	pub_ <- T.readFile acc
	pub_2 <- T.readFile acc'
	sec <- chomp <$> T.readFile foo
	if (scr == "secure") 
	then runSecureClient raddr (read rprt) "/" (ws (readOpt opt) sec pub_ pub_2 fsnd msg)
	else runClient raddr (read rprt) "/" (ws (readOpt opt) sec pub_ pub_2 fsnd msg)

ws :: Opt -> T.Text -> T.Text -> T.Text -> Bool -> String -> ClientApp ()
ws opt sec pub_ pub_2 fsnd msg cnn = do
	putStrLn "Connected!\n"

	Just pub <- pure . dataPart $ chomp pub_
	let	req0 = A.encode . A.Array . V.fromList
			. ([A.String "REQ", A.String "foobar12345"] ++) . (: [])
			. FlJsn.encode <$> mkFilter opt pub_2
	maybe (pure ()) BSLC.putStrLn req0
	maybe (pure ()) (sendTextData cnn) req0
	putStrLn ""

	r <- receiveData cnn
	BSLC.putStrLn r
	putStrLn ""

	case A.decode r of
		Just (A.Array (toList -> [A.String "EOSE", A.String "foobar12345"])) -> do
			putStrLn "EOSE RECEIVED"
		Just (A.Array (toList -> [
			A.String "EVENT", A.String "foobar12345", A.Object obj ])) -> do
			let	ev = EvJs.decode obj
			-- print ev
			-- putStrLn ""
			maybe (pure ()) (T.putStrLn . Event.content) ev
		_ -> error "bad"

	putStrLn "\n*** WRITE ***"

	when fsnd $ write sec pub_ cnn msg

	doWhile do

		rdt <- receiveData cnn
		BSLC.putStrLn rdt
		let	r' = A.decode rdt :: Maybe A.Value -- <$> receiveData cnn :: IO (Maybe A.Value)

		pure case r' of
			Just (A.Array (V.toList -> [A.String "EOSE", A.String "foobar12345"])) -> False
			Just _ -> True
			Nothing -> error "bad"

	sendClose cnn ("Bye!" :: T.Text)

mkFilter :: Opt -> T.Text -> Maybe Filter
mkFilter opt a = do
	pub <- dataPart (chomp a)
	pk <- Event.publicFromBech32 a
	pure case opt of
		Author -> Filter {
			ids = Nothing,
			authors = Just [pk], kinds = Just [1], tags = [],
			since = Nothing, until = Nothing, limit = Just 5 }
		P -> Filter {
			ids = Nothing,
			authors = Nothing, kinds = Just [1],
			tags = [('p', [T.pack . strToHexStr $ BSC.unpack pub])],
			since = Nothing, until = Nothing, limit = Just 5 }

write :: T.Text -> T.Text -> Connection -> String -> IO ()
write sec pub cnn msg = do
	Just pk <- pure $ Event.publicFromBech32 pub
	ut <- getUnixTime
	Just sec' <- pure $ Event.secretFromBech32 sec
	json <- EvJs.encode sec' Event.E {
		Event.pubkey = pk, Event.created_at = ut,
		Event.kind = 1, Event.tags = [], Event.content = T.pack msg }
	print json
	print $ EvJs.decode json
	let	event = A.encode . A.Array $ V.fromList [A.String "EVENT", A.Object json]
	BSLC.putStrLn event
	sendTextData cnn event
