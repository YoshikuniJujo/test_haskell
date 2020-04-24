{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handle (
	handle, FollowboxM, FollowboxState, initialFollowboxState ) where

import Prelude hiding ((++))

import Control.Monad.State
import Data.Type.Set
import Data.Bool
import Data.String
import Data.UnionSet hiding (merge)
import Data.Time
import Data.Aeson
import System.Process
import System.Random

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Simple as HTTP

import Network.HTTP.Simple

import qualified Data.ByteString.Lazy as LBS

import MonadicFrp.Handle
import Trials.Followbox.Event hiding (getTimeZone)
import Field

import Trials.Followbox.ThreadId
import Trials.Followbox.Random
import Trials.Followbox.TypeSynonym

type FollowboxM = StateT FollowboxState
data FollowboxState = FollowboxState {
	fsObjects :: [Object],
	fsSleepUntil :: Maybe UTCTime,
	fsVersionRandomGens :: [(StdGenVersion, StdGen)] }
	deriving Show

initialFollowboxState :: FollowboxState
initialFollowboxState = FollowboxState {
	fsObjects = [],
	fsSleepUntil = Nothing,
	fsVersionRandomGens = initialVersionStdGens $ mkStdGen 8 }

instance RandomState FollowboxState where
	getVersionStdGen = head . fsVersionRandomGens
	putVersionStdGen s (v, g) = s { fsVersionRandomGens = (v, g) : fsVersionRandomGens s }
	rollbackStdGen s@FollowboxState { fsVersionRandomGens = (v0, _) : vgs } v
		| v == v0 = Right s { fsVersionRandomGens = vgs }
		| otherwise = Left "can't rollback"
	rollbackStdGen _ _ = Left "can't rollback"

getObjects :: Monad m => FollowboxM m [Object]
getObjects = gets fsObjects

putObjects :: Monad m => [Object] -> FollowboxM m ()
putObjects os = modify \s -> s { fsObjects = os }

getSleepUntil :: Monad m => FollowboxM m (Maybe UTCTime)
getSleepUntil = gets fsSleepUntil

putSleepUntil :: Monad m => Maybe UTCTime -> FollowboxM m ()
putSleepUntil slp = modify \s -> s { fsSleepUntil = slp }

handleHttpGet :: Maybe (BS.ByteString, BS.ByteString) -> EvReqs (Singleton HttpGet) -> IO (EvOccs (Singleton HttpGet))
handleHttpGet mba reqs = do
	r <- hg . setUserAgent "Yoshio" $ fromString u
	print $ HTTP.getResponseHeader "X-RateLimit-Remaining" r
	pure . singleton $ OccHttpGet u (HTTP.getResponseHeaders r) (HTTP.getResponseBody r)
	where
	hg = case mba of
		Just (nm, tkn) -> httpBasicAuth nm tkn
		Nothing -> HTTP.httpLBS
	HttpGetReq u = extract reqs

setUserAgent :: BS.ByteString -> HTTP.Request -> HTTP.Request
setUserAgent ua = HTTP.setRequestHeader "User-Agent" [ua]

handleStoreJsons :: Monad m => EvReqs (Singleton StoreJsons) -> FollowboxM m (EvOccs (Singleton StoreJsons))
handleStoreJsons reqs = singleton (OccStoreJsons os) <$ putObjects os
	where StoreJsons os = extract reqs

handleLoadJsons :: Monad m => EvReqs (Singleton LoadJsons) -> FollowboxM m (EvOccs (Singleton LoadJsons))
handleLoadJsons _reqs = singleton . OccLoadJsons <$> getObjects

handleRaiseError :: EvReqs (Singleton RaiseError) -> IO (Maybe (EvOccs (Singleton RaiseError)))
handleRaiseError reqs = case e of
	NotJson -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Terminate
	EmptyJson -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Continue
	NoLoginName -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Terminate
	NoAvatarAddress -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Terminate
	NoAvatar -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Terminate
	NoHtmlUrl -> do
		putStrLn $ "ERROR: " <> em
		pure . Just . singleton $ OccRaiseError e Terminate
	CatchError -> pure Nothing
	where RaiseError e em = extract reqs

handle :: Field -> Browser -> Maybe (GithubUserName, GithubToken) -> Handle (FollowboxM IO) FollowboxEv
handle f brws mba = retry $
	handleGetThreadId `merge`
	handleRandom `merge`
	(Just <$>) . handleStoreJsons `merge`
	liftIO . (Just <$>) . handleHttpGet mba `merge`
	(Just <$>) . handleLoadJsons `merge`
	liftIO . (Just <$>) . handleCalcTextExtents f `merge`
	liftIO . handleRaiseError `merge`
	handleBeginSleep `merge`
	handleEndSleep `merge`
	liftIO . (Just <$>) . handleGetTimeZone `merge`
	liftIO . (Just <$>) . handleBrowse brws `before`
	handleLeftClick f

handleLeftClick :: Field -> EvReqs (Move :- LeftClick :- Quit :- 'Nil) -> (FollowboxM IO) (Maybe (EvOccs (Move :- LeftClick :- Quit :- 'Nil)))
handleLeftClick f reqs = getSleepUntil >>= liftIO . \case
	Nothing -> handleLeftClick' f reqs
	Just t -> handleLeftClickUntil f t reqs

handleLeftClick' :: Field -> EvReqs (Move :- LeftClick :- Quit :- 'Nil) -> IO (Maybe (EvOccs (Move :- LeftClick :- Quit :- 'Nil)))
handleLeftClick' f _reqs = withNextEvent f \case
	DestroyWindowEvent {} -> pure . Just . expand $ singleton OccQuit
	ExposeEvent {} -> flushField f >> pure Nothing
	ButtonEvent { ev_event_type = 4, ev_button = 1, ev_x = x, ev_y = y } ->
		pure . Just $ expand (OccMove (fromIntegral x, fromIntegral y) >- singleton OccLeftClick :: EvOccs (Move :- LeftClick :- 'Nil))
	ButtonEvent { ev_event_type = 4, ev_button = 3 } -> pure . Just . expand . singleton $ OccQuit
	MotionEvent { ev_x = x, ev_y = y } -> pure . Just . expand . singleton $ OccMove (fromIntegral x, fromIntegral y)
	ev	| isDeleteEvent f ev -> destroyField f >> pure Nothing
		| otherwise -> pure Nothing

handleLeftClickUntil :: Field -> UTCTime -> EvReqs (Move :- LeftClick :- Quit :- 'Nil) ->
	IO (Maybe (EvOccs (Move :- LeftClick :- Quit :- 'Nil)))
handleLeftClickUntil f t _reqs = withNextEventUntil f t \case
	Just DestroyWindowEvent {} -> pure . Just . expand $ singleton OccQuit
	Just ExposeEvent { } -> flushField f >> pure Nothing
	Just ButtonEvent { ev_event_type = 4, ev_button = 1, ev_x = x, ev_y = y } ->
		pure . Just $ expand (OccMove (fromIntegral x, fromIntegral y) >- singleton OccLeftClick :: EvOccs (Move :- LeftClick :- 'Nil))
	Just ButtonEvent { ev_event_type = 4, ev_button = 3 } -> pure . Just . expand . singleton $ OccQuit
	Just MotionEvent { ev_x = x, ev_y = y } -> pure . Just . expand . singleton $ OccMove (fromIntegral x, fromIntegral y)
	Just ev	| isDeleteEvent f ev -> destroyField f >> pure Nothing
		| otherwise -> pure Nothing
	Nothing -> pure Nothing

withNextEventUntil :: Field -> UTCTime -> (Maybe Event -> IO a) -> IO a
withNextEventUntil f t act = do
	now <- getCurrentTime
	let	dt = t `diffUTCTime` now
	withNextEventTimeout' f (round $ dt * 1000000) act

handleCalcTextExtents :: Field -> EvReqs (Singleton CalcTextExtents) -> IO (EvOccs (Singleton CalcTextExtents))
handleCalcTextExtents f reqs = singleton . OccCalcTextExtents fn fs t <$> textExtents f fn fs (T.unpack t)
	where CalcTextExtentsReq fn fs t = extract reqs

handleBeginSleep :: Monad m => EvReqs (Singleton BeginSleep) -> FollowboxM m (Maybe (EvOccs (Singleton BeginSleep)))
handleBeginSleep reqs = case extract reqs of
	BeginSleep t -> do
		getSleepUntil >>= \case
			Just t' -> pure . Just . singleton $ OccBeginSleep t'
			Nothing -> do
				putSleepUntil $ Just t
				pure . Just . singleton $ OccBeginSleep t
	CheckBeginSleep -> pure Nothing

handleEndSleep :: EvReqs (Singleton EndSleep) -> FollowboxM IO (Maybe (EvOccs (Singleton EndSleep)))
handleEndSleep _reqs = getSleepUntil >>= \case
	Just t -> do
		now <- liftIO getCurrentTime
		bool	(pure Nothing)
			(putSleepUntil Nothing >> pure (Just $ singleton OccEndSleep))
			(t <= now)
	Nothing -> pure . Just $ singleton OccEndSleep

handleGetTimeZone :: EvReqs (Singleton GetTimeZone) -> IO (EvOccs (Singleton GetTimeZone))
handleGetTimeZone _reqs = do
	tz <- getCurrentTimeZone
	pure . singleton $ OccGetTimeZone tz

handleBrowse :: FilePath -> Handle IO (Singleton Browse)
handleBrowse brws reqs = spawnProcess brws [u] >> pure (singleton OccBrowse)
	where Browse u = extract reqs

httpBasicAuth :: BS.ByteString -> BS.ByteString -> Request -> IO (Response LBS.ByteString)
httpBasicAuth usr p rq = httpLBS $ setRequestBasicAuth usr p rq
