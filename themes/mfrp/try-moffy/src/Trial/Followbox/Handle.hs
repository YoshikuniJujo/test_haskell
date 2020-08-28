{-# LANGUAGE BlockArguments, LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.Handle (
	-- * HANDLE
	handleFollowbox',
	-- * STATE
	HandleF, FollowboxState, initialFollowboxState ) where

import Prelude hiding (head)

import Control.Moffy.Handle
import Data.Type.Set (Singleton, (:+:))
import Data.OneOrMore (pattern Singleton)
import Data.Bool (bool)
import Data.List (delete)
import Data.String (fromString)
import Data.Aeson (Object)
import Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone, diffUTCTime)
import System.Random (StdGen)
import System.Process (spawnProcess)

import qualified Data.Text as T
import qualified Network.HTTP.Simple as H

import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.Lock (LockState(..), LockId, handleLock)
import Control.Moffy.Handle.Random (RandomState(..), handleRandom)
import Control.Moffy.Handle.XField
import Trial.Followbox.Event (
	FollowboxEv, StoreJsons(..), LoadJsons,
	HttpGet(..), CalcTextExtents(..), GetTimeZone, Browse(..),
	BeginSleep(..), EndSleep, RaiseError(..), Error(..), ErrorResult(..),
	pattern OccStoreJsons, pattern OccLoadJsons,
	pattern OccHttpGet, pattern OccCalcTextExtents,
	pattern OccGetTimeZone, pattern OccBrowse,
	pattern OccBeginSleep, pattern OccEndSleep, pattern OccRaiseError
	)
import Trial.Followbox.TypeSynonym (Browser, GithubNameToken)
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

data FollowboxState = FollowboxState {
	fsNextLockId :: Int, fsLockState :: [LockId], fsObjects :: [Object],
	fsSleepUntil :: Maybe UTCTime, fsRandomGen :: StdGen } deriving Show

initialFollowboxState :: StdGen -> FollowboxState
initialFollowboxState g = FollowboxState {
	fsNextLockId = 0, fsLockState = [], fsObjects = [],
	fsSleepUntil = Nothing, fsRandomGen = g }

type HandleF m es = HandleSt FollowboxState m es
type HandleF' m es = HandleIo' FollowboxState FollowboxState m es

-- PUT AND GET EACH STATE

instance LockState FollowboxState where
	getNextLockId = fsNextLockId; putNextLockId s l = s { fsNextLockId = l }
	isLocked s l = l `elem` fsLockState s
	lockIt s l = s { fsLockState = l : fsLockState s }
	unlockIt s l = s { fsLockState = delete l $ fsLockState s }

instance RandomState FollowboxState where
	getRandomGen = fsRandomGen; putRandomGen s g = s { fsRandomGen = g }

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

-- FOLLOWBOX

handleFollowbox' ::
	Field -> Browser -> Maybe GithubNameToken -> HandleF IO (GuiEv :+: FollowboxEv)
handleFollowbox' f brws mba = retrySt $
	liftSt . handleGetThreadId `mergeSt` handleLock `mergeSt` handleRandom `mergeSt`
	handleStoreJsons' `mergeSt` handleLoadJsons' `mergeSt`
	liftSt . just . handleHttpGet mba `mergeSt`
	liftSt . just . handleCalcTextExtents f `mergeSt`
	liftSt . just . handleGetTimeZone `mergeSt`
	liftSt . just . handleBrowse brws `mergeSt`
	handleBeginSleep' `mergeSt` handleEndSleep' `mergeSt`
	liftSt . handleRaiseError `beforeSt`
	handleMouseWithSleep' f

-- MOUSE

handleMouseWithSleep' :: Field -> HandleF' IO GuiEv
handleMouseWithSleep' f rqs s = (, s) <$> case fsSleepUntil s of
	Nothing -> handle Nothing f rqs
	Just t -> getCurrentTime >>= \now ->
		handle (Just . realToFrac $ t `diffUTCTime` now) f rqs

-- STORE AND LOAD JSONS

handleStoreJsons' :: Monad m => HandleF' m (Singleton StoreJsons)
handleStoreJsons' (Singleton (StoreJsonsReq os)) s =
	pure (Just . Singleton $ OccStoreJsons os, s { fsObjects = os })

handleLoadJsons' :: Monad m => HandleF' m (Singleton LoadJsons)
handleLoadJsons' _rqs s = pure (Just . Singleton . OccLoadJsons $ fsObjects s, s)

-- REQUEST DATA

handleHttpGet :: Maybe GithubNameToken -> Handle IO (Singleton HttpGet)
handleHttpGet mgnt (Singleton (HttpGetReq u)) = do
	r <- H.httpLBS . maybe id (uncurry H.setRequestBasicAuth) mgnt
		. H.setRequestHeader "User-Agent" ["Yoshio"]
		. fromString $ T.unpack u
	print $ H.getResponseHeader "X-RateLimit-Remaining" r
	pure . Singleton
		$ OccHttpGet u (H.getResponseHeaders r) (H.getResponseBody r)

handleCalcTextExtents :: Field -> Handle IO (Singleton CalcTextExtents)
handleCalcTextExtents f (Singleton (CalcTextExtentsReq fn fs t)) = Singleton
	. OccCalcTextExtents fn fs t <$> textExtents f fn fs (T.unpack t)

handleGetTimeZone :: Handle IO (Singleton GetTimeZone)
handleGetTimeZone _reqs = Singleton . OccGetTimeZone <$> getCurrentTimeZone

-- BROWSE

handleBrowse :: Browser -> Handle IO (Singleton Browse)
handleBrowse brws (Singleton (Browse u)) = Singleton OccBrowse <$ spawnProcess brws [T.unpack u]

-- BEGIN AND END SLEEP

handleBeginSleep' :: Monad m => HandleF' m (Singleton BeginSleep)
handleBeginSleep' (Singleton bs) s = case bs of
	BeginSleep t -> case fsSleepUntil s of
		Just t' -> pure (Just . Singleton $ OccBeginSleep t', s)
		Nothing -> pure (Just . Singleton $ OccBeginSleep t, s { fsSleepUntil = Just t })
	CheckBeginSleep -> pure (Nothing, s)

handleEndSleep' :: HandleF' IO (Singleton EndSleep)
handleEndSleep' _rqs s = case fsSleepUntil s of
	Just t -> getCurrentTime >>= bool
		(pure (Nothing, s))
		(pure (Just $ Singleton OccEndSleep, s { fsSleepUntil = Nothing })) . (t <=)
	Nothing -> pure (Just $ Singleton OccEndSleep, s)

-- RAISE ERROR

handleRaiseError :: Handle' IO (Singleton RaiseError)
handleRaiseError (Singleton (RaiseError e em)) = case er e of
	Nothing -> pure Nothing
	Just r -> Just (Singleton $ OccRaiseError e r) <$ putStrLn emsg
	where
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
