{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handle (
	handle, FbM, FollowboxState, initialFollowboxState ) where

import Prelude hiding (head)

import Control.Monad.State (StateT, lift, gets, modify)
import Data.Type.Set (Set(Nil), Singleton, (:-))
import Data.List.NonEmpty (NonEmpty(..), head, cons)
import Data.UnionSet (singleton, (>-), extract, expand)
import Data.Bool (bool)
import Data.String (fromString)
import Data.Aeson (Object)
import Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone, diffUTCTime)
import System.Random (StdGen, mkStdGen)
import System.Process (spawnProcess)

import qualified Data.Text as T
import qualified Network.HTTP.Simple as H

import MonadicFrp.Handle
import Trials.Followbox.Event hiding (getTimeZone)
import Trials.Followbox.Random
import Trials.Followbox.ThreadId
import Trials.Followbox.TypeSynonym (Browser, GithubNameToken)
import Field (
	Field, Event(..), flushField, destroyField, isDeleteEvent,
	withNextEvent, withNextEventTimeout', textExtents )

---------------------------------------------------------------------------

type FbM = StateT FollowboxState
data FollowboxState = FollowboxState {
	fsObjects :: [Object],
	fsSleepUntil :: Maybe UTCTime,
	fsVersionRandomGens :: NonEmpty (StdGenVersion, StdGen) }
	deriving Show

initialFollowboxState :: FollowboxState
initialFollowboxState = FollowboxState {
	fsObjects = [], fsSleepUntil = Nothing,
	fsVersionRandomGens = (stdGenVersion0, mkStdGen 8) :| [] }

instance RandomState FollowboxState where
	getVersionStdGen = head . fsVersionRandomGens
	putVersionStdGen s (v, g) =
		s { fsVersionRandomGens = (v, g) `cons` fsVersionRandomGens s }
	rollbackStdGen
		s@FollowboxState { fsVersionRandomGens = (v0, _) :| vg : vgs } v
		| v == v0 = Right s { fsVersionRandomGens = vg :| vgs }
		| otherwise = Left "can't rollback"
	rollbackStdGen _ _ = Left "can't rollback"

getObjects :: Monad m => FbM m [Object]
getObjects = gets fsObjects

putObjects :: Monad m => [Object] -> FbM m ()
putObjects os = modify \s -> s { fsObjects = os }

getSleepUntil :: Monad m => FbM m (Maybe UTCTime)
getSleepUntil = gets fsSleepUntil

putSleepUntil :: Monad m => Maybe UTCTime -> FbM m ()
putSleepUntil slp = modify \s -> s { fsSleepUntil = slp }

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

handle ::
	Field -> Browser -> Maybe GithubNameToken -> Handle (FbM IO) FollowboxEv
handle f brws mba = retry $
	handleGetThreadId `merge` handleRandom `merge`
	handleStoreLoadJsons `merge`
	lift . just . handleHttpGet mba `merge`
	lift . just . handleCalcTextExtents f `merge`
	lift . just . handleGetTimeZone `merge`
	lift . just . handleBrowse brws `merge`
	handleBeginEndSleep `merge` lift . handleRaiseError `before`
	handleMouse f

just :: Functor f => f a -> f (Maybe a)
just = (Just <$>)

-- STORE AND LOAD JSONS

type StoreLoadJsons = StoreJsons :- LoadJsons :- 'Nil

handleStoreLoadJsons :: Monad m => Handle' (FbM m) StoreLoadJsons
handleStoreLoadJsons = just . handleStoreJsons `merge` just . handleLoadJsons

handleStoreJsons :: Monad m => Handle (FbM m) (Singleton StoreJsons)
handleStoreJsons reqs = singleton (OccStoreJsons os) <$ putObjects os
	where StoreJsons os = extract reqs

handleLoadJsons :: Monad m => Handle (FbM m) (Singleton LoadJsons)
handleLoadJsons _reqs = singleton . OccLoadJsons <$> getObjects

-- HTTP GET

handleHttpGet :: Maybe GithubNameToken -> Handle IO (Singleton HttpGet)
handleHttpGet mgnt reqs = do
	r <- H.httpLBS . maybe id (uncurry H.setRequestBasicAuth) mgnt
		. H.setRequestHeader "User-Agent" ["Yoshio"]
		. fromString $ T.unpack u
	print $ H.getResponseHeader "X-RateLimit-Remaining" r
	pure . singleton
		$ OccHttpGet u (H.getResponseHeaders r) (H.getResponseBody r)
	where HttpGetReq u = extract reqs

-- CALC TEXT EXTENTS

handleCalcTextExtents :: Field -> EvReqs (Singleton CalcTextExtents) -> IO (EvOccs (Singleton CalcTextExtents))
handleCalcTextExtents f reqs = singleton . OccCalcTextExtents fn fs t <$> textExtents f fn fs (T.unpack t)
	where CalcTextExtentsReq fn fs t = extract reqs

-- GET TIME ZONE

handleGetTimeZone :: EvReqs (Singleton GetTimeZone) -> IO (EvOccs (Singleton GetTimeZone))
handleGetTimeZone _reqs = do
	tz <- getCurrentTimeZone
	pure . singleton $ OccGetTimeZone tz

-- BROWSE

handleBrowse :: FilePath -> Handle IO (Singleton Browse)
handleBrowse brws reqs = spawnProcess brws [T.unpack u] >> pure (singleton OccBrowse)
	where Browse u = extract reqs

-- BEGIN AND END SLEEP

type BeginEndSleepEv = BeginSleep :- EndSleep :- 'Nil

handleBeginEndSleep :: EvReqs BeginEndSleepEv -> FbM IO (Maybe (EvOccs BeginEndSleepEv))
handleBeginEndSleep = handleBeginSleep `merge` handleEndSleep

handleBeginSleep :: Monad m => EvReqs (Singleton BeginSleep) -> FbM m (Maybe (EvOccs (Singleton BeginSleep)))
handleBeginSleep reqs = case extract reqs of
	BeginSleep t -> do
		getSleepUntil >>= \case
			Just t' -> pure . Just . singleton $ OccBeginSleep t'
			Nothing -> do
				putSleepUntil $ Just t
				pure . Just . singleton $ OccBeginSleep t
	CheckBeginSleep -> pure Nothing

handleEndSleep :: EvReqs (Singleton EndSleep) -> FbM IO (Maybe (EvOccs (Singleton EndSleep)))
handleEndSleep _reqs = getSleepUntil >>= \case
	Just t -> do
		now <- lift getCurrentTime
		bool	(pure Nothing)
			(putSleepUntil Nothing >> pure (Just $ singleton OccEndSleep))
			(t <= now)
	Nothing -> pure . Just $ singleton OccEndSleep

-- RAISE ERROR

handleRaiseError :: EvReqs (Singleton RaiseError) -> IO (Maybe (EvOccs (Singleton RaiseError)))
handleRaiseError reqs = case errorResult e of
	Nothing -> pure Nothing
	Just r -> do
		putStrLn ("ERROR: " <> em)
		pure . Just . singleton $ OccRaiseError e r
	where
	RaiseError e em = extract reqs
	errorResult = \case
		NoRateLimitRemaining -> Just Terminate
		NoRateLimitReset -> Just Terminate
		NotJson -> Just Terminate
		EmptyJson -> Just Continue
		NoLoginName -> Just Terminate
		NoAvatarAddress -> Just Terminate
		NoAvatar -> Just Terminate
		NoHtmlUrl -> Just Terminate
		CatchError -> Nothing

-- MOUSE

handleMouse :: Field -> EvReqs (Move :- LeftClick :- Quit :- 'Nil) -> (FbM IO) (Maybe (EvOccs (Move :- LeftClick :- Quit :- 'Nil)))
handleMouse f reqs = getSleepUntil >>= lift . \case
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
