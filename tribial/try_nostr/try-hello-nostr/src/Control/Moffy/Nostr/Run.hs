{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

{-# LANGUAGE ExplicitForAll, RequiredTypeArguments #-}

module Control.Moffy.Nostr.Run (

	run, ws, PreNowEnd(..)

	) where

import Prelude hiding (break, scanl)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Nostr.Event
import Data.Type.Set
import Data.OneOrMore qualified as OOM
import Data.OneOrMoreApp
import Data.Vector qualified as V
import Data.Text qualified as T
import Data.Aeson qualified as A
import System.Timeout
import Network.WebSockets qualified as Ws
import Wuss qualified as Ws
import Nostr.Event.Json qualified as EvJsn
import Nostr.Filter.Json qualified as FlJsn
import Tools

run :: (TVar PreNowEnd -> a -> IO ()) ->
	String -> String -> Sig s Events a r -> IO ()
run pr rad rprt s = do
	ve <- atomically $ newTVar Pre
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO . void $ interpret (handleCh cr co) (pr ve) s
	Ws.runSecureClient rad (read rprt) "/" $ ws cr co ve
	where handleCh cr co rq =
		atomically (writeTChan cr rq) >> atomically (readTChan co)

ws :: TChan (EvReqs Events) ->
	TChan (EvOccs Events) -> TVar PreNowEnd -> Ws.ClientApp ()
ws cr co vend cnn = do
	doWhile $ atomically (readTChan cr) >>= \r -> do
		proj () r \(ReqReq nm f) ->
			Ws.sendTextData cnn (req nm f) >> occ Req co OccReq
		proj () r \EventReq -> void . timeout 1000000 $ readPost co cnn
		proj () r \EndReq -> flip when (occ (type End) co OccEnd)
			. (== End) =<< atomically (readTVar vend)
		proj True r \HaltReq -> False <$ occ Halt co OccHalt
	Ws.sendClose cnn ("Bye!" :: T.Text)
	where
	proj d = flip (maybe $ pure d) . OOM.project
	req nm = A.encode . A.Array . V.fromList
		. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode

readPost :: TChan (EvOccs Events) -> Ws.Connection -> IO ()
readPost co cnn = Ws.receiveData cnn >>= \rdt -> case A.decode rdt of
	Just (A.Array (V.toList -> [A.String "EOSE", A.String nm])) ->
		occ Eose co $ OccEose nm
	Just (A.Array (V.toList ->
			[A.String "EVENT", A.String nm, A.Object jsn ])) -> do
		Just ev <- pure $ EvJsn.decode jsn
		occ Event co $ OccEvent nm ev
	Just _ -> pure (); Nothing -> error "bad"

data PreNowEnd = Pre | Now | End deriving (Show, Eq)

occ :: forall e -> Expandable Occurred (Singleton e) Events =>
	TChan (EvOccs Events) -> Occurred e -> IO ()
occ e co = atomically
	. writeTChan co . expand @(Singleton (Occurred e)) . Singleton
