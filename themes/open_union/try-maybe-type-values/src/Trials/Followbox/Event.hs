{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Trials.Followbox.Event (
	-- * GENERAL
	SigF, ReactF, FollowboxEv, FollowboxEvGen, Occurred(..),

	-- * MOUSE EVENT
	MouseEv, MouseMove, MouseDown, mouseMove, leftClick,

	-- * STORE AND LOAD
	-- ** Jsons
	StoreJsons(..), LoadJsons, clearJsons, storeJsons, loadJsons,

	-- * REQUEST DATA
	-- ** HttpGet
	HttpGet(..), httpGet,
	-- ** CalcTextExtents
	CalcTextExtents(..), calcTextExtents,
	-- ** GetTimeZone
	GetTimeZone, getTimeZone,

	-- * ACTION
	-- ** Browse
	Browse(..), browse,

	-- * SLEEP, QUIT AND ERROR
	-- ** BeginSleep and EndSleep
	BeginSleep(..), EndSleep, beginSleep, checkBeginSleep, endSleep,
	-- ** Quit
	deleteEvent,
	-- ** RaiseError
	RaiseError(..), Error(..), ErrorResult(..),
	raiseError, checkTerminate
	) where

import Data.Type.Set (numbered, Singleton, Set(Nil), (:-), (:+:))
import Data.UnionSet (Mrgable(..))
import Data.Bool (bool)
import Data.Time (UTCTime, TimeZone)
import Data.Aeson (Object)
import Network.HTTP.Simple (Header)
import Graphics.X11.Xrender (XGlyphInfo)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import MonadicFrp (Request(..), Sig, React, await)
import MonadicFrp.EventHandle.ThreadId (GetThreadId)
import MonadicFrp.EventHandle.Lock
import MonadicFrp.Event.Mouse
import Trials.Followbox.Random (RandomEv)
import Trials.Followbox.TypeSynonym (
	Uri, FontName, FontSize, ErrorMessage )

---------------------------------------------------------------------------

data Result = Failure | Succeed deriving Show

result :: a -> a -> Result -> a
result f s = \case Failure -> f; Succeed -> s

---------------------------------------------------------------------------
-- STORE AND LOAD
---------------------------------------------------------------------------

-- JSONS

data StoreJsons = StoreJsons [Object] deriving Show
numbered 9 [t| StoreJsons |]
instance Mrgable StoreJsons where os1 `mrg` _os2 = os1
instance Request StoreJsons where
	data Occurred StoreJsons = OccStoreJsons [Object]

storeJsons :: [Object] -> React (Singleton StoreJsons) ()
storeJsons os = result (storeJsons os) (pure ()) =<< await (StoreJsons os)
	\(OccStoreJsons os') -> bool Failure Succeed $ os == os'

clearJsons :: React (Singleton StoreJsons) ()
clearJsons = storeJsons []

data LoadJsons = LoadJsonsReq deriving (Show, Eq, Ord)
numbered 9 [t| LoadJsons |]
instance Request LoadJsons where data Occurred LoadJsons = OccLoadJsons [Object]

loadJsons :: React (Singleton LoadJsons) [Object]
loadJsons = await LoadJsonsReq \(OccLoadJsons os) -> os

---------------------------------------------------------------------------
-- REQUEST DATA
---------------------------------------------------------------------------

-- HTTP GET

data HttpGet = HttpGetReq Uri deriving (Show, Eq, Ord)
numbered 9 [t| HttpGet |]
instance Request HttpGet where
	data Occurred HttpGet = OccHttpGet Uri [Header] LBS.ByteString
		deriving Show

httpGet :: Uri -> React (Singleton HttpGet) ([Header], LBS.ByteString)
httpGet u = maybe (httpGet u) pure =<< await (HttpGetReq u)
	\(OccHttpGet u' hs c) -> bool Nothing (Just (hs, c)) $ u == u'

-- CALC TEXT EXTENTS

data CalcTextExtents = CalcTextExtentsReq FontName FontSize T.Text
	deriving (Show, Eq, Ord)
numbered 9 [t| CalcTextExtents |]
instance Request CalcTextExtents where
	data Occurred CalcTextExtents =
		OccCalcTextExtents FontName FontSize T.Text XGlyphInfo

calcTextExtents :: FontName -> FontSize -> T.Text ->
	React (Singleton CalcTextExtents) XGlyphInfo
calcTextExtents fn fs t = maybe (calcTextExtents fn fs t) pure
	=<< await (CalcTextExtentsReq fn fs t)
		\(OccCalcTextExtents fn' fs' t' glp) ->
			bool Nothing (Just glp)
				$ fn == fn' && fs == fs' && t == t'

-- GET TIME ZONE

data GetTimeZone = GetTimeZone deriving (Show, Eq, Ord)
numbered 9 [t| GetTimeZone |]
instance Request GetTimeZone where
	data Occurred GetTimeZone = OccGetTimeZone TimeZone deriving Show

getTimeZone :: React (Singleton GetTimeZone) TimeZone
getTimeZone = await GetTimeZone \(OccGetTimeZone tz) -> tz

---------------------------------------------------------------------------
-- ACTION
---------------------------------------------------------------------------

-- BROWSE

data Browse = Browse Uri deriving (Show, Eq, Ord)
numbered 9 [t| Browse |]
instance Request Browse where data Occurred Browse = OccBrowse deriving Show

browse :: Uri -> React (Singleton Browse) ()
browse u = await (Browse u) \OccBrowse -> ()

---------------------------------------------------------------------------
-- SLEEP, QUIT AND ERROR
---------------------------------------------------------------------------

-- BEGIN SLEEP AND END SLEEP

data BeginSleep = BeginSleep UTCTime | CheckBeginSleep deriving (Show, Eq, Ord)
numbered 9 [t| BeginSleep |]
instance Request BeginSleep where
	data Occurred BeginSleep = OccBeginSleep UTCTime deriving Show

beginSleep :: UTCTime -> React (Singleton BeginSleep) ()
beginSleep t = result (beginSleep t) (pure ()) =<< await (BeginSleep t) \case
	OccBeginSleep t' | t == t' -> Succeed; _ -> Failure

checkBeginSleep :: React (Singleton BeginSleep) UTCTime
checkBeginSleep = await CheckBeginSleep \case OccBeginSleep t -> t

data EndSleep = EndSleepReq deriving (Show, Eq, Ord)
numbered 9 [t| EndSleep |]
instance Request EndSleep where
	data Occurred EndSleep = OccEndSleep deriving Show

endSleep :: React (Singleton EndSleep) ()
endSleep = await EndSleepReq \OccEndSleep -> ()

-- ERROR

data Error
	= NoRateLimitRemaining | NoRateLimitReset
	| NotJson | EmptyJson | NoLoginName | NoAvatarAddress | NoAvatar
	| NoHtmlUrl | CatchError deriving (Show, Eq, Ord)

data ErrorResult = Continue | Terminate deriving Show

data RaiseError = RaiseError Error ErrorMessage deriving (Show, Eq, Ord)
numbered 9 [t| RaiseError |]
instance Request RaiseError where
	data Occurred RaiseError = OccRaiseError Error ErrorResult

raiseError :: Error -> ErrorMessage -> React (Singleton RaiseError) ()
raiseError e em = bool (raiseError e em) (pure ()) =<< await (RaiseError e em)
	\(OccRaiseError e' _er) -> e == e'

catchError :: React (Singleton RaiseError) ErrorResult
catchError = await (RaiseError CatchError "") \(OccRaiseError _ er) -> er

checkTerminate :: React (Singleton RaiseError) ()
checkTerminate = catchError >>= \case
	Continue -> checkTerminate; Terminate -> pure ()

---------------------------------------------------------------------------
-- GENERAL
---------------------------------------------------------------------------

type SigF = Sig FollowboxEv
type ReactF = React FollowboxEv

type FollowboxEv = GetThreadId :- LockEv :+: RandomEv :+: MouseEv :+: FollowboxEvGen

type FollowboxEvGen =
	StoreJsons :- LoadJsons :-
	HttpGet :- CalcTextExtents :- GetTimeZone :- Browse :-
	BeginSleep :- EndSleep :- RaiseError :- 'Nil
