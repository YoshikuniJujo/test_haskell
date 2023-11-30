{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Control.Moffy.Samples.Followbox.Event (
	-- * Followbox Event
	SigF, ReactF, FollowboxEv,
	-- * Store and Load Jsons
	StoreJsons(..), pattern OccStoreJsons, LoadJsons, pattern OccLoadJsons,
	clearJsons, storeJsons, loadJsons,
	-- * Request Data
	-- ** Http Get
	HttpGet(..), pattern OccHttpGet, httpGet,
--	-- ** Calc Text Extents
--	CalcTextExtents(..), pattern OccCalcTextExtents, calcTextExtents,
	-- ** Get Time Zone
	GetTimeZone, pattern OccGetTimeZone, getTimeZone,
	-- * Browse
	Browse(..), pattern OccBrowse, browse,
	-- * Sleep
	BeginSleep(..), pattern OccBeginSleep, EndSleep, pattern OccEndSleep,
	beginSleep, checkBeginSleep, endSleep,
	-- * Raise Error
	RaiseError(..), pattern OccRaiseError, Error(..), ErrorResult(..),
	raiseError, checkTerminate ) where

import Control.Moffy (Sig, React, Request(..), await)
import Control.Moffy.Event.ThreadId (GetThreadId)
import Control.Moffy.Event.Lock (LockEv)
import Control.Moffy.Event.Random (RandomEv)
import Control.Moffy.Samples.Event.Delete (DeleteEvent)
import Control.Moffy.Samples.Event.Mouse qualified as Mouse (Move, Down, Up)
import Control.Moffy.Samples.Followbox.Event.CalcTextExtents
import Data.Type.Set (Set(Nil), Singleton, numbered, (:-), (:+:))
import Data.OneOrMore (Selectable(..))
import Data.Bool (bool)
import Data.Aeson (Object)
import Data.Time (UTCTime, TimeZone)
import Network.HTTP.Simple (Header)

import qualified Data.ByteString.Lazy as LBS

import Trial.Followbox.TypeSynonym (Uri, ErrorMessage)

---------------------------------------------------------------------------

-- * STORE AND LOAD JSON OBJECT LIST
-- * REQUEST DATA
-- 	+ HTTP GET
--	+ CALC TEXT EXTENTS
--	+ TIME ZONE
-- * BROWSE
-- * SLEEP
-- * RAISE ERROR
-- * FOLLOWBOX EVENT TYPE

---------------------------------------------------------------------------
-- STORE AND LOAD JSON OBJECT LIST
---------------------------------------------------------------------------

newtype StoreJsons = StoreJsonsReq [Object] deriving Show
numbered [t| StoreJsons |]
instance Selectable StoreJsons where l `select` _r = l
instance Request StoreJsons where
	data Occurred StoreJsons = OccStoreJsons [Object]

clearJsons :: React s (Singleton StoreJsons) ()
clearJsons = storeJsons []

storeJsons :: [Object] -> React s (Singleton StoreJsons) ()
storeJsons os = bool (storeJsons os) (pure ())
	=<< await (StoreJsonsReq os) \(OccStoreJsons os') -> os == os'

data LoadJsons = LoadJsonsReq deriving (Show, Eq, Ord)
numbered [t| LoadJsons |]
instance Request LoadJsons where data Occurred LoadJsons = OccLoadJsons [Object]

loadJsons :: React s (Singleton LoadJsons) [Object]
loadJsons = await LoadJsonsReq \(OccLoadJsons os) -> os

---------------------------------------------------------------------------
-- REQUEST DATA
---------------------------------------------------------------------------

-- HTTP GET

newtype HttpGet = HttpGetReq Uri deriving (Show, Eq, Ord)
numbered [t| HttpGet |]
instance Request HttpGet where
	data Occurred HttpGet = OccHttpGet Uri [Header] LBS.ByteString

httpGet :: Uri -> React s (Singleton HttpGet) ([Header], LBS.ByteString)
httpGet u = maybe (httpGet u) pure =<< await (HttpGetReq u)
	\(OccHttpGet u' hs c) -> bool Nothing (Just (hs, c)) $ u == u'

-- TIME ZONE

data GetTimeZone = GetTimeZoneReq deriving (Show, Eq, Ord)
numbered [t| GetTimeZone |]
instance Request GetTimeZone where
	data Occurred GetTimeZone = OccGetTimeZone TimeZone deriving Show

getTimeZone :: React s (Singleton GetTimeZone) TimeZone
getTimeZone = await GetTimeZoneReq \(OccGetTimeZone tz) -> tz

---------------------------------------------------------------------------
-- BROWSE
---------------------------------------------------------------------------

newtype Browse = Browse Uri deriving (Show, Eq, Ord)
numbered [t| Browse |]
instance Request Browse where data Occurred Browse = OccBrowse deriving Show

browse :: Uri -> React s (Singleton Browse) ()
browse u = await (Browse u) \OccBrowse -> ()

---------------------------------------------------------------------------
-- SLEEP
---------------------------------------------------------------------------

data BeginSleep = BeginSleep UTCTime | CheckBeginSleep deriving (Show, Eq, Ord)
numbered [t| BeginSleep |]
instance Request BeginSleep where
	data Occurred BeginSleep = OccBeginSleep UTCTime deriving Show

beginSleep :: UTCTime -> React s (Singleton BeginSleep) ()
beginSleep t = bool (beginSleep t) (pure ()) =<< await (BeginSleep t) \case
	OccBeginSleep t' | t == t' -> True; _ -> False

checkBeginSleep :: React s (Singleton BeginSleep) UTCTime
checkBeginSleep = await CheckBeginSleep \case OccBeginSleep t -> t

data EndSleep = EndSleepReq deriving (Show, Eq, Ord)
numbered [t| EndSleep |]
instance Request EndSleep where
	data Occurred EndSleep = OccEndSleep deriving Show

endSleep :: React s (Singleton EndSleep) ()
endSleep = await EndSleepReq \OccEndSleep -> ()

---------------------------------------------------------------------------
-- RAISE ERROR
---------------------------------------------------------------------------

data Error
	= NoRateLimitRemaining | NoRateLimitReset
	| NotJson | EmptyJson | NoLoginName | NoAvatarAddress | NoAvatar
	| NoHtmlUrl | Trace | CatchError deriving (Show, Eq, Ord)

data ErrorResult = Continue | Terminate deriving Show

data RaiseError = RaiseError Error ErrorMessage deriving (Show, Eq, Ord)
numbered [t| RaiseError |]
instance Request RaiseError where
	data Occurred RaiseError = OccRaiseError Error ErrorResult

raiseError :: Error -> ErrorMessage -> React s (Singleton RaiseError) ()
raiseError e em = bool (raiseError e em) (pure ())
	=<< await (RaiseError e em) \(OccRaiseError e' _er) -> e == e'

catchError :: React s (Singleton RaiseError) ErrorResult
catchError = await (RaiseError CatchError "") \(OccRaiseError _ er) -> er

checkTerminate :: React s (Singleton RaiseError) ()
checkTerminate = catchError
	>>= \case Continue -> checkTerminate; Terminate -> pure ()

---------------------------------------------------------------------------
-- FOLLOWBOX EVENT TYPE
---------------------------------------------------------------------------

type SigF s = Sig s FollowboxEv
type ReactF s r = React s FollowboxEv r

type FollowboxEv = GetThreadId :- LockEv :+: RandomEv :+: DeleteEvent :- MouseEv :+:
	StoreJsons :- LoadJsons :- HttpGet :- CalcTextExtents :- GetTimeZone :-
	Browse :- BeginSleep :- EndSleep :- RaiseError :- 'Nil

type MouseEv = Mouse.Move :- Mouse.Down :- Mouse.Up :- 'Nil
