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
import Control.Moffy.Handle qualified as H
import Control.Moffy.Run
import Control.Moffy.Samples.Handle.TChan
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
	waitFor . adjust $ await (ReqReq "foobar" Filter.Filter {
		Filter.ids = Nothing, Filter.authors = Nothing,
		Filter.kinds = Just [1], Filter.tags = [],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }) id
	emit =<< waitFor (adjust $ await EventReq \(OccEvent nm ev) -> ev)
	waitFor . adjust $ await HaltReq id

run :: Show a => String -> String -> Sig s Events a r -> IO ()
run raddr rprt s = do
	cr <- atomically newTChan
	co <- atomically newTChan
	forkIO $ do
		interpret (H.retry $ handle (Just 0.1) cr co) print s
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws "foo" cr co

ws :: T.Text ->
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> Ws.ClientApp ()
ws pub cr co cnn = do
	hlt <- atomically $ newTVar False
	forkIO . doWhile $ atomically (readTChan cr) >>= \r -> do
--		threadDelay 1000000
		case OOM.project r of
			Nothing -> pure ()
			Just (ReqReq nm fltr) -> do
				let	r1 = req nm fltr
				Ws.sendTextData cnn r1
				atomically . writeTChan co . expand $ Singleton OccReq
		case OOM.project r of
			Nothing -> pure ()
			Just HaltReq -> do
				atomically $ writeTVar hlt True
				atomically . writeTChan co . expand $ Singleton OccHalt
		pure True
	doWhile $ Ws.receiveData cnn >>= \rdt -> do
--		threadDelay 1000000
		threadDelay 100000
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
		atomically $ not <$> readTVar hlt

req :: T.Text -> Filter.Filter -> LBSC.ByteString
req nm = A.encode . A.Array . V.fromList
	. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode
