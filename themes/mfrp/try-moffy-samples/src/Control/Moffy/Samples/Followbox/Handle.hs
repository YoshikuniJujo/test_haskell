{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox.Handle (
	-- * Handle
	HandleF, HandleF', handleFollowboxWith, GuiEv,
	-- * State
	FollowboxState(..), initialFollowboxState,
	) where

import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.CalcTextExtents (CalcTextExtents)
import Control.Moffy.Handle (
	Handle, Handle', HandleSt, HandleSt', HandleIo', liftHandle',
	retrySt, beforeSt, mergeSt )
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.Lock (LockState(..), LockId, handleLock)
import Control.Moffy.Samples.Handle.Random (RandomState(..), handleRandom)
import Data.Type.Set (Singleton, (:-), (:+:), pattern Nil)
import Data.OneOrMore as Oom (pattern Singleton)
import Data.Bool (bool)
import Data.List (delete)
import Data.String (fromString)
import Data.Aeson (Object)
import Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone, diffUTCTime, DiffTime)
import System.Random (StdGen)
import System.Process (spawnProcess)

import qualified Data.Text as T
import qualified Network.HTTP.Simple as H

import Control.Moffy.Samples.Followbox.Event (
	FollowboxEv, StoreJsons(..), pattern OccStoreJsons,
	LoadJsons, pattern OccLoadJsons, HttpGet(..), pattern OccHttpGet,
	GetTimeZone, pattern OccGetTimeZone, Browse(..), pattern OccBrowse,
	BeginSleep(..), pattern OccBeginSleep, EndSleep, pattern OccEndSleep,
	RaiseError(..), pattern OccRaiseError, Error(..), ErrorResult(..) )
import Control.Moffy.Samples.Followbox.TypeSynonym (Browser, GithubNameToken)

import Data.OneOrMoreApp as Ooma

---------------------------------------------------------------------------

-- * STATE
-- 	+ FOLLOWBOX STATE
-- 	+ PUT AND GET EACH STATE
-- * HANDLE
--	+ FOLLOWBOX
--	+ MOUSE
--	+ STORE AND LOAD JSONS
--	+ REQUEST DATA
--	+ BROWSE
--	+ BEGIN AND END SLEEP
--	+ RAISE ERROR
-- * HELPER FUNCTION

---------------------------------------------------------------------------
-- STATE
---------------------------------------------------------------------------

type GuiEv = DeleteEvent :- MouseEv

type MouseEv = Mouse.Move :- Mouse.Down :- Mouse.Up :- 'Nil

-- FOLLOWOBOX STATE

data FollowboxState = FollowboxState {
	fsNextLockId :: Int, fsLockState :: [LockId], fsObjects :: [Object],
	fsSleepUntil :: Maybe UTCTime, fsRandomGen :: StdGen
	} deriving Show

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

handleFollowboxWith ::
	(Maybe DiffTime -> f -> Handle' IO (CalcTextExtents :- GuiEv)) ->
	f -> Browser -> Maybe GithubNameToken ->
	HandleF IO (GuiEv :+: FollowboxEv)
handleFollowboxWith h f brws mba = retrySt $
	liftHandle' handleGetThreadId `mergeSt` handleLock `mergeSt`
	handleRandom `mergeSt`
	handleStoreJsons `mergeSt` handleLoadJsons `mergeSt`
	liftOnJust (handleHttpGet mba) `mergeSt`
	liftOnJust handleGetTimeZone `mergeSt`
	liftOnJust (handleBrowse brws) `mergeSt`
	handleBeginSleep `mergeSt` handleEndSleep `mergeSt`
	liftHandle' handleRaiseError `beforeSt` handleMouseWithSleep h f

-- MOUSE

handleMouseWithSleep ::
	(Maybe DiffTime -> f -> Handle' IO (CalcTextExtents :- GuiEv)) ->
	f -> HandleF' IO (CalcTextExtents :- GuiEv)
handleMouseWithSleep h f rqs s = (, s) <$> case fsSleepUntil s of
	Nothing -> h Nothing f rqs
	Just t -> getCurrentTime >>= \now ->
		h (Just . realToFrac $ t `diffUTCTime` now) f rqs

-- STORE AND LOAD JSONS

handleStoreJsons :: Monad m => HandleF' m (Singleton StoreJsons)
handleStoreJsons (Oom.Singleton (StoreJsonsReq os)) s =
	pure (Just . Ooma.Singleton $ OccStoreJsons os, s { fsObjects = os })

handleLoadJsons :: Monad m => HandleF' m (Singleton LoadJsons)
handleLoadJsons _rqs s = pure (Just . Ooma.Singleton . OccLoadJsons $ fsObjects s, s)

-- REQUEST DATA

handleHttpGet :: Maybe GithubNameToken -> Handle IO (Singleton HttpGet)
handleHttpGet mgnt (Oom.Singleton (HttpGetReq u)) = do
	r <- H.httpLBS . maybe id (uncurry H.setRequestBasicAuth) mgnt
		. H.setRequestHeader "User-Agent" ["Yoshio"]
		. fromString $ T.unpack u
	print $ H.getResponseHeader "X-RateLimit-Remaining" r
	pure . Ooma.Singleton
		$ OccHttpGet u (H.getResponseHeaders r) (H.getResponseBody r)

handleGetTimeZone :: Handle IO (Singleton GetTimeZone)
handleGetTimeZone _reqs = Ooma.Singleton . OccGetTimeZone <$> getCurrentTimeZone

-- BROWSE

handleBrowse :: Browser -> Handle IO (Singleton Browse)
handleBrowse brws (Oom.Singleton (Browse u)) =
	Ooma.Singleton OccBrowse <$ spawnProcess brws [T.unpack u]

-- BEGIN AND END SLEEP

handleBeginSleep :: Monad m => HandleF' m (Singleton BeginSleep)
handleBeginSleep (Oom.Singleton bs) s = case bs of
	BeginSleep t -> case fsSleepUntil s of
		Just t' -> pure (Just . Ooma.Singleton $ OccBeginSleep t', s)
		Nothing -> pure (
			Just . Ooma.Singleton $ OccBeginSleep t,
			s { fsSleepUntil = Just t } )
	CheckBeginSleep -> pure (Nothing, s)

handleEndSleep :: HandleF' IO (Singleton EndSleep)
handleEndSleep _rqs s = case fsSleepUntil s of
	Just t -> getCurrentTime >>= bool
		(pure (Nothing, s))
		(pure (Just $ Ooma.Singleton OccEndSleep,
			s { fsSleepUntil = Nothing })) . (t <=)
	Nothing -> pure (Just $ Ooma.Singleton OccEndSleep, s)

-- RAISE ERROR

handleRaiseError :: Handle' IO (Singleton RaiseError)
handleRaiseError (Oom.Singleton (RaiseError e em)) = case er e of
	Nothing -> pure Nothing
	Just r -> Just (Ooma.Singleton $ OccRaiseError e r) <$ putStrLn emsg
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

---------------------------------------------------------------------------
-- HELPER FUNCTION
---------------------------------------------------------------------------

liftOnJust :: Functor f => Handle f es -> HandleSt' st f es
liftOnJust = liftHandle' . ((Just <$>) .)
