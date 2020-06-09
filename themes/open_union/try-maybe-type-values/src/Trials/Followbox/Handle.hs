{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handle (
	-- * HANDLE
	handleFollowbox,
	-- * STATE
	FbM, FollowboxState, initialFollowboxState ) where

import Prelude hiding (head)

import Control.Monad.State (StateT, lift, gets, modify)
import Data.Type.Set (Singleton)
import Data.UnionSet (singleton, extract)
import Data.Bool (bool)
import Data.List (delete)
import Data.String (fromString)
import Data.Aeson (Object)
import Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone, diffUTCTime)
import System.Random (StdGen)
import System.Process (spawnProcess)

import qualified Data.Text as T
import qualified Network.HTTP.Simple as H

import MonadicFrp.Handle (Handle, Handle', retry, merge, before)
import MonadicFrp.EventHandle.ThreadId (handleGetThreadId)
import MonadicFrp.EventHandle.Lock (LockState(..), LockId, handleLock)
import MonadicFrp.EventHandle.Random (RandomState(..), handleRandom)
import MonadicFrp.Event.Mouse (MouseEv)
import MonadicFrp.XFieldHandle.Mouse (handleMouse)
import Trials.Followbox.Event (
	FollowboxEv, Occurred(..), StoreJsons(..), LoadJsons,
	HttpGet(..), CalcTextExtents(..), GetTimeZone, Browse(..),
	BeginSleep(..), EndSleep, RaiseError(..), Error(..), ErrorResult(..) )
import Trials.Followbox.TypeSynonym (Browser, GithubNameToken)
import Field (Field, textExtents)

---------------------------------------------------------------------------

-- * STATE
-- 	+ FULLOWBOX STATE
-- 	+ PUT AND GET EACH STATE
-- * HANDLE
--	+ FOLLOWBOX
--	+ MOUSE
--	+ STORE AND LOAD JSONS
--	+ REQUEST DATA
--	+ BROWSE
--	+ BEGIN AND END SLEEP
--	+ RAISE ERROR
--	+ HELPER FUNCTION

---------------------------------------------------------------------------
-- STATE
---------------------------------------------------------------------------

-- FOLLOWQBOX STATE

type FbM = StateT FollowboxState
data FollowboxState = FollowboxState {
	fsNextLockId :: Int, fsLockState :: [LockId], fsObjects :: [Object],
	fsSleepUntil :: Maybe UTCTime, fsRandomGen :: StdGen } deriving Show

initialFollowboxState :: StdGen -> FollowboxState
initialFollowboxState g = FollowboxState {
	fsNextLockId = 0, fsLockState = [], fsObjects = [],
	fsSleepUntil = Nothing, fsRandomGen = g }

-- PUT AND GET EACH STATE

instance LockState FollowboxState where
	getNextLockId = fsNextLockId; putNextLockId s l = s { fsNextLockId = l }
	isLocked s l = l `elem` fsLockState s
	lockIt s l = s { fsLockState = l : fsLockState s }
	unlockIt s l = s { fsLockState = delete l $ fsLockState s }

instance RandomState FollowboxState where
	getRandomGen = fsRandomGen; putRandomGen s g = s { fsRandomGen = g }

getObjects :: Monad m => FbM m [Object]
getObjects = gets fsObjects

putObjects :: Monad m => [Object] -> FbM m ()
putObjects os = modify \s -> s { fsObjects = os }

getSleepUntil :: Monad m => FbM m (Maybe UTCTime)
getSleepUntil = gets fsSleepUntil

putSleepUntil :: Monad m => UTCTime -> FbM m ()
putSleepUntil slp = modify \s -> s { fsSleepUntil = Just slp }

resetSleep :: Monad m => FbM m ()
resetSleep = modify \s -> s { fsSleepUntil = Nothing }

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

-- FOLLOWBOX

handleFollowbox ::
	Field -> Browser -> Maybe GithubNameToken -> Handle (FbM IO) FollowboxEv
handleFollowbox f brws mba = retry $
	handleGetThreadId `merge` handleLock `merge` handleRandom `merge`
	just . handleStoreJsons `merge` just . handleLoadJsons `merge`
	lift . just . handleHttpGet mba `merge`
	lift . just . handleCalcTextExtents f `merge`
	lift . just . handleGetTimeZone `merge`
	lift . just . handleBrowse brws `merge`
	handleBeginSleep `merge` handleEndSleep `merge`
	lift . handleRaiseError `before`
	handleMouseWithSleep f

-- MOUSE

handleMouseWithSleep :: Field -> Handle' (FbM IO) MouseEv
handleMouseWithSleep f reqs = getSleepUntil >>= lift . \case
	Nothing -> handleMouse Nothing f reqs
	Just t -> getCurrentTime >>= \now ->
		handleMouse (Just . realToFrac $ t `diffUTCTime` now) f reqs

-- STORE AND LOAD JSONS

handleStoreJsons :: Monad m => Handle (FbM m) (Singleton StoreJsons)
handleStoreJsons reqs = singleton (OccStoreJsons os) <$ putObjects os
	where StoreJsons os = extract reqs

handleLoadJsons :: Monad m => Handle (FbM m) (Singleton LoadJsons)
handleLoadJsons _reqs = singleton . OccLoadJsons <$> getObjects

-- REQUEST DATA

handleHttpGet :: Maybe GithubNameToken -> Handle IO (Singleton HttpGet)
handleHttpGet mgnt reqs = do
	r <- H.httpLBS . maybe id (uncurry H.setRequestBasicAuth) mgnt
		. H.setRequestHeader "User-Agent" ["Yoshio"]
		. fromString $ T.unpack u
	print $ H.getResponseHeader "X-RateLimit-Remaining" r
	pure . singleton
		$ OccHttpGet u (H.getResponseHeaders r) (H.getResponseBody r)
	where HttpGetReq u = extract reqs

handleCalcTextExtents :: Field -> Handle IO (Singleton CalcTextExtents)
handleCalcTextExtents f reqs = singleton
	. OccCalcTextExtents fn fs t <$> textExtents f fn fs (T.unpack t)
	where CalcTextExtentsReq fn fs t = extract reqs

handleGetTimeZone :: Handle IO (Singleton GetTimeZone)
handleGetTimeZone _reqs = singleton . OccGetTimeZone <$> getCurrentTimeZone

-- BROWSE

handleBrowse :: Browser -> Handle IO (Singleton Browse)
handleBrowse brws reqs = singleton OccBrowse <$ spawnProcess brws [T.unpack u]
	where Browse u = extract reqs

-- BEGIN AND END SLEEP

handleBeginSleep :: Monad m => Handle' (FbM m) (Singleton BeginSleep)
handleBeginSleep reqs = case extract reqs of
	BeginSleep t -> getSleepUntil >>= \case
		Just t' -> pure . Just . singleton $ OccBeginSleep t'
		Nothing -> Just (singleton $ OccBeginSleep t) <$ putSleepUntil t
	CheckBeginSleep -> pure Nothing

handleEndSleep :: Handle' (FbM IO) (Singleton EndSleep)
handleEndSleep _reqs = getSleepUntil >>= \case
	Just t -> lift getCurrentTime >>=
		bool (pure Nothing) (e <$ resetSleep) . (t <=)
	Nothing -> pure e
	where e = Just $ singleton OccEndSleep

-- RAISE ERROR

handleRaiseError :: Handle' IO (Singleton RaiseError)
handleRaiseError reqs = case er e of
	Nothing -> pure Nothing
	Just r -> Just (singleton $ OccRaiseError e r) <$ putStrLn emsg
	where
	RaiseError e em = extract reqs
	emsg = "ERROR: " <> em
	er = \case
		NoRateLimitRemaining -> Just Terminate
		NoRateLimitReset -> Just Terminate
		NotJson -> Just Terminate
		EmptyJson -> Just Continue
		NoLoginName -> Just Terminate
		NoAvatarAddress -> Just Terminate
		NoAvatar -> Just Terminate
		NoHtmlUrl -> Just Terminate
		Trace -> Just Continue
		CatchError -> Nothing

-- HELPER FUNCTION

just :: Functor f => f a -> f (Maybe a)
just = (Just <$>)
