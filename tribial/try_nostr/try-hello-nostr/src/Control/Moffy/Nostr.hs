{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Nostr where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Run
import Data.Type.Set
import Data.OneOrMore qualified as OOM
import Data.OneOrMoreApp
import Data.Vector qualified as V
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Aeson qualified as A
import Nostr.Event qualified as Event
import Nostr.Event.Json qualified as EvJsn
import Nostr.Filter qualified as Filter
import Nostr.Filter.Json qualified as FlJsn
import Wuss qualified as Ws
import Network.WebSockets qualified as Ws

import Tools

data Req = ReqReq T.Text Filter.Filter deriving (Show, Eq, Ord)
numbered [t| Req |]
instance Request Req where
	data Occurred Req = OccReq deriving Show

data Event = EventReq deriving (Show, Eq, Ord)
numbered [t| Event |]
instance Request Event where
	data Occurred Event = OccEvent T.Text Event.E deriving Show

data Eose = EoseReq deriving (Show, Eq, Ord)
numbered [t| Eose |]
instance Request Eose where
	data Occurred Eose = OccEose T.Text deriving Show

data Halt = HaltReq deriving (Show, Eq, Ord)
numbered [t| Halt |]
instance Request Halt where
	data Occurred Halt = OccHalt deriving Show

type Events = Req :- Event :- Eose :- Halt :- 'Nil

sample :: IO ()
sample = run "nos.lol" "443" $ do
	_ <- waitFor . adjust $ await (ReqReq "foobar" Filter.Filter {
		Filter.ids = Nothing, Filter.authors = Nothing,
		Filter.kinds = Just [1], Filter.tags = [],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }) id
	emit =<< waitFor (adjust $ await EventReq \(OccEvent _nm ev) -> ev)
	waitFor . adjust $ await HaltReq id

handleTChan :: TChan (EvReqs es) -> TChan (EvOccs es) -> Handle IO es
handleTChan cr co rq = do
	atomically $ writeTChan cr rq
	atomically $ readTChan co

run :: Show a => String -> String -> Sig s Events a r -> IO ()
run raddr rprt s = do
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO $ do
		_ <- interpret (handleTChan cr co) print s
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws "foo" cr co

ws :: T.Text ->
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> Ws.ClientApp ()
ws _pub cr co cnn = do
	doWhile $ atomically (readTChan cr) >>= \r -> do
		a <- case OOM.project r of
			Nothing -> pure True
			Just (ReqReq nm fltr) -> do
				let	r1 = req nm fltr
				Ws.sendTextData cnn r1
				atomically . writeTChan co . expand $ Singleton OccReq
				pure True
		b <- case OOM.project r of
			Nothing -> pure True
			Just HaltReq -> do
				atomically . writeTChan co . expand $ Singleton OccHalt
				pure False
		c <- case OOM.project r of
			Nothing -> pure True
			Just EventReq -> do
				readPost co cnn
				pure True
		print $ a && b && c
		pure $ a && b && c

readPost :: TChan (EvOccs Events) -> Ws.Connection -> IO ()
readPost co cnn = Ws.receiveData cnn >>= \rdt -> do
		LBSC.putStrLn rdt
		case A.decode rdt of
			Just (A.Array (V.toList -> [
					A.String "EOSE",
					A.String nm ])) ->
				atomically . writeTChan co . expand . Singleton $ OccEose nm
			Just (A.Array (V.toList -> [
					A.String "EVENT",
					A.String nm,
					A.Object jsn ])) -> do
				Just ev <- pure $ EvJsn.decode jsn
				atomically . writeTChan co . expand
					. Singleton $ OccEvent nm ev
			Just _ -> pure (); Nothing -> error "bad"

req :: T.Text -> Filter.Filter -> LBSC.ByteString
req nm = A.encode . A.Array . V.fromList
	. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode
