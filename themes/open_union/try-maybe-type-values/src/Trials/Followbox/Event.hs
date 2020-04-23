{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Trials.Followbox.Event (
	-- * GENERAL
	SigF, ReactF, FollowboxEv, Occurred(..),

	-- * MOUSE EVENT
	-- ** Move
	Move, move,
	-- ** LeftClick
	LeftClick, leftClick,

	-- * STORE AND LOAD
	-- ** RandomGen
	getRandomR,
	-- ** Jsons
	StoreJsons(..), LoadJsons, Object, Value(..), storeJsons, loadJsons,

	-- * REQUEST DATA
	-- ** HttpGet
	HttpGet(..), Uri, Header, httpGet,
	-- ** CalcTextExtents
	CalcTextExtents(..), XGlyphInfo(..), FontName, FontSize, calcTextExtents,
	-- ** GetTimeZone
	GetTimeZone, getTimeZone,

	-- * ACTION
	-- ** Browse
	Browse(..), browse,

	-- * SLEEP, QUIT AND ERROR
	-- ** BeginSleep and EndSleep
	BeginSleep(..), EndSleep, beginSleep, checkBeginSleep, endSleep,
	-- ** Quit
	Quit, checkQuit,
	-- ** RaiseError
	RaiseError(..), Error(..), ErrorMessage, ErrorResult(..),
	raiseError, catchError
	) where

import Data.Type.Set
import Data.UnionSet
import Data.Bool
import Data.Time hiding (getTimeZone)
import Data.Aeson hiding (Result)
import Network.HTTP.Simple (Header)
import Graphics.X11.Xrender

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import MonadicFrp

import Trials.Followbox.ThreadId
import Trials.Followbox.Random

data Move = MoveReq deriving (Show, Eq, Ord)
numbered 8 [t| Move |]
instance Request Move where
	data Occurred Move = OccMove (Integer, Integer) deriving Show
-- type Position = (Integer, Integer)

move :: React (Singleton Move) (Integer, Integer)
move = await MoveReq \(OccMove p) -> p

data Result = Failure | Succeed deriving Show

result :: a -> a -> Result -> a
result f _ Failure = f
result _ s Succeed = s

data LeftClick = LeftClickReq deriving (Show, Eq, Ord)
numbered 8 [t| LeftClick |]
instance Request LeftClick where
	data Occurred LeftClick = OccLeftClick deriving Show

leftClick :: React (Singleton LeftClick) ()
leftClick = await LeftClickReq \OccLeftClick -> ()

data HttpGet = HttpGetReq Uri deriving (Show, Eq, Ord)
type Uri = String
numbered 8 [t| HttpGet |]
instance Request HttpGet where
	data Occurred HttpGet = OccHttpGet Uri [Header] LBS.ByteString
		deriving Show

httpGet :: Uri -> React (Singleton HttpGet) ([Header], LBS.ByteString)
httpGet u = maybe (httpGet u) pure =<< await (HttpGetReq u)
	\(OccHttpGet u' hs c) -> bool Nothing (Just (hs, c)) $ u == u'

data StoreJsons = StoreJsons [Object] deriving Show
numbered 8 [t| StoreJsons |]
instance Mrgable StoreJsons where os1 `mrg` _os2 = os1
instance Request StoreJsons where
	data Occurred StoreJsons = OccStoreJsons [Object]

storeJsons :: [Object] -> React (Singleton StoreJsons) ()
storeJsons os = result (storeJsons os) (pure ()) =<< await (StoreJsons os)
	\(OccStoreJsons os') -> bool Failure Succeed $ os == os'

data LoadJsons = LoadJsonsReq deriving (Show, Eq, Ord)
numbered 8 [t| LoadJsons |]
instance Request LoadJsons where
	data Occurred LoadJsons = OccLoadJsons [Object]

loadJsons :: React (Singleton LoadJsons) [Object]
loadJsons = await LoadJsonsReq \(OccLoadJsons os) -> os

data CalcTextExtents = CalcTextExtentsReq FontName FontSize T.Text
	deriving (Show, Eq, Ord)
type FontName = String
type FontSize = Double
numbered 8 [t| CalcTextExtents |]
instance Request CalcTextExtents where
	data Occurred CalcTextExtents =
		OccCalcTextExtents String Double T.Text XGlyphInfo

calcTextExtents :: FontName -> FontSize -> T.Text ->
	React (Singleton CalcTextExtents) XGlyphInfo
calcTextExtents fn fs t = maybe (calcTextExtents fn fs t) pure
	=<< await (CalcTextExtentsReq fn fs t)
		\(OccCalcTextExtents fn' fs' t' glp) ->
			bool Nothing (Just glp)
				$ fn == fn' && fs == fs' && t == t'

data BeginSleep = BeginSleep UTCTime | CheckBeginSleep deriving (Show, Eq, Ord)
numbered 8 [t| BeginSleep |]
instance Request BeginSleep where
	data Occurred BeginSleep = OccBeginSleep UTCTime deriving Show

beginSleep :: UTCTime -> React (Singleton BeginSleep) ()
beginSleep t = result (beginSleep t) (pure ()) =<< await (BeginSleep t) \case
	OccBeginSleep t' | t == t' -> Succeed
	_ -> Failure

checkBeginSleep :: React (Singleton BeginSleep) UTCTime
checkBeginSleep = await CheckBeginSleep \case OccBeginSleep t -> t

data EndSleep = EndSleepReq deriving (Show, Eq, Ord)
numbered 8 [t| EndSleep |]
instance Request EndSleep where
	data Occurred EndSleep = OccEndSleep deriving Show

endSleep :: React (Singleton EndSleep) ()
endSleep = await EndSleepReq \OccEndSleep -> ()

data GetTimeZone = GetTimeZone deriving (Show, Eq, Ord)
numbered 8 [t| GetTimeZone |]
instance Request GetTimeZone where
	data Occurred GetTimeZone = OccGetTimeZone TimeZone deriving Show

getTimeZone :: React (Singleton GetTimeZone) TimeZone
getTimeZone = await GetTimeZone \(OccGetTimeZone tz) -> tz

data Browse = Browse Uri deriving (Show, Eq, Ord)
numbered 8 [t| Browse |]
instance Request Browse where data Occurred Browse = OccBrowse deriving Show

browse :: Uri -> React (Singleton Browse) ()
browse u = await (Browse u) \OccBrowse -> ()

data Quit = QuitReq deriving (Show, Eq, Ord)
numbered 8 [t| Quit |]
instance Request Quit where data Occurred Quit = OccQuit

checkQuit :: React (Singleton Quit) ()
checkQuit = await QuitReq $ const ()

data Error
	= NotJson | EmptyJson | NoLoginName | NoAvatarAddress | NoAvatar | NoHtmlUrl | CatchError
	deriving (Show, Eq, Ord)

data ErrorResult = Continue | Terminate deriving Show

data RaiseError = RaiseError Error ErrorMessage deriving (Show, Eq, Ord)
type ErrorMessage = String
numbered 8 [t| RaiseError |]
instance Request RaiseError where
	data Occurred RaiseError = OccRaiseError Error ErrorResult

raiseError :: Error -> ErrorMessage -> React (Singleton RaiseError) ()
raiseError e em = bool (raiseError e em) (pure ()) =<< await (RaiseError e em)
	\(OccRaiseError e' _er) -> e == e'

catchError :: React (Singleton RaiseError) ErrorResult
catchError = await (RaiseError CatchError "") \(OccRaiseError _ er) -> er

type SigF = Sig FollowboxEv
type ReactF = React FollowboxEv

type FollowboxEvGen =
	Move :- LeftClick :-
	StoreJsons :- LoadJsons :-
	HttpGet :- CalcTextExtents :- GetTimeZone :- Browse :-
	BeginSleep :- EndSleep :- Quit :- RaiseError :- 'Nil

type FollowboxEv = GetThreadId :- FollowboxEvGen :+: RandomEv
