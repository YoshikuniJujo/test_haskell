{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Nostr2 (sampleId2) where

import Prelude hiding (break, scanl)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Nostr.Event
import Data.OneOrMore qualified as OOM
import Data.OneOrMoreApp
import Data.Vector qualified as V
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Aeson qualified as A
import System.Timeout
import Network.WebSockets qualified as Ws
import Wuss qualified as Ws
import Nostr.Event qualified as Event
import Nostr.Event.Json qualified as EvJsn
import Nostr.Filter qualified as Filter
import Nostr.Filter.Json qualified as FlJsn
import Tools
import TryBech32

sampleId2 :: FilePath -> IO ()
sampleId2 fp = do
	Just flt <- filterP <$> T.readFile fp
	run display "nos.lol" "443" do
		waitFor $ request "req-p" flt
		void $ parList (spawn evPair) `break` end
		waitFor halt
	where
	display e ms = prp `mapM_` ms >> atomically case ms of
		[] -> readTVar e >>= \case Now -> writeTVar e End; _ -> pure ()
		_ -> readTVar e >>= \case Pre -> writeTVar e Now; _ -> pure ()
	prp (ev, mev) = do
		print ev
		T.putStrLn $ Event.content ev
		print $ lookupE ev
		case mev of
			Nothing -> putStrLn "NO ROOT MESSAGE"
			Just ev' -> print ev' >> T.putStrLn (Event.content ev')
	evPair = waitFor (awaitNameEvent "req-p") >>= \ev -> case lookupE ev of
		Nothing -> emit (ev, Nothing)
		Just e -> do
			emit (ev, Nothing)
			waitFor $ request nm (filterId e)
			emit . (ev ,) . Just =<< waitFor (awaitNameEvent nm)
			where nm = "id-" <> e

run :: (TVar PreNowEnd -> a -> IO ()) -> String -> String -> Sig s Events a r -> IO ()
run pr raddr rprt s = do
	vend <- atomically $ newTVar Pre
	cr <- atomically newTChan
	co <- atomically newTChan
	_ <- forkIO $ do
		_ <- interpret (handleTChan cr co) (pr vend) s
		pure ()
	Ws.runSecureClient raddr (read rprt) "/" $ ws "foo" cr co vend
	where
	handleTChan :: TChan (EvReqs es) -> TChan (EvOccs es) -> Handle IO es
	handleTChan cr co rq = do
		atomically $ writeTChan cr rq
		atomically $ readTChan co

ws :: T.Text ->
	TChan (EvReqs Events) -> TChan (EvOccs Events) ->
	TVar PreNowEnd -> Ws.ClientApp ()
ws _pub cr co vend cnn = do
	doWhile $ atomically (readTChan cr) >>= \r -> do
		a <- case OOM.project r of
			Nothing -> pure True
			Just (ReqReq nm fltr) -> do
				T.putStrLn nm
				let	r1 = req nm fltr
				Ws.sendTextData cnn r1
				atomically . writeTChan co . expand $ Singleton OccReq
				pure True
		b <- case OOM.project r of
			Nothing -> pure True
			Just HaltReq -> do
				putStrLn "HALT"
				atomically . writeTChan co . expand $ Singleton OccHalt
				pure False
		c <- case OOM.project r of
			Nothing -> pure True
			Just EventReq -> do
				_ <- timeout 1000000 $ readPost co cnn
				pure True
		_ <- case OOM.project r of
			Nothing -> pure True
			Just EndReq -> do
				e' <- atomically $ readTVar vend
				atomically . when (e' == End) . writeTChan co . expand $ Singleton OccEnd
				pure True
		pure $ a && b && c
	putStrLn "BYEBYE"
	Ws.sendClose cnn ("Bye!" :: T.Text)
	where
	req :: T.Text -> Filter.Filter -> LBSC.ByteString
	req nm = A.encode . A.Array . V.fromList
		. ([A.String "REQ", A.String nm] ++) . (: []) . FlJsn.encode

readPost :: TChan (EvOccs Events) -> Ws.Connection -> IO ()
readPost co cnn = Ws.receiveData cnn >>= \rdt -> do
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

data PreNowEnd = Pre | Now | End deriving (Show, Eq)

lookupE :: Event.E -> Maybe T.Text
lookupE = (fst <$>) . lookup "e" . Event.tags

filterP :: T.Text -> Maybe Filter.Filter
filterP a = do
	pub <- toHex <$> dataPart' "npub" (chomp a)
	pure Filter.Filter {
		Filter.ids = Nothing,
		Filter.authors = Nothing, Filter.kinds = Just [1],
		Filter.tags = [('p', [pub])],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }

filterId :: T.Text -> Filter.Filter
filterId a = Filter.Filter {
	Filter.ids = Just [fromHex a],
	Filter.authors = Nothing, Filter.kinds = Just [1],
	Filter.tags = [],
	Filter.since = Nothing, Filter.until = Nothing,
	Filter.limit = Just 1 }
