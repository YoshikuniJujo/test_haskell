{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.Event (
	ReactF, FollowboxEvent(..), Object, Value(..),
	XGlyphInfo(..), FontName, FontSize, Uri,
	isHttpGet, isSleepUntil, decodeJson,
	move, leftClick, syncWaitMessage,
	storeRandoms, loadRandoms, storeJsons, loadJsons,
	storeRateLimitReset, loadRateLimitReset,
	getCurrentTime, getCurrentTimeZone, calcTextExtents, httpGet,
	browse, sleepUntil, raiseError ) where

import Prelude hiding (map, repeat, until)
import GHC.Stack (HasCallStack)
import Data.Time (UTCTime, TimeZone)
import Network.HTTP.Simple (Header)

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS

import React (React, EvOccs, exper)
import Event (Event(..), Action(..), Bidirectional(..))
import Followbox.AesonObject (Object, Value(..), decodeJson)

---------------------------------------------------------------------------

type ReactF s n r = React s (FollowboxEvent n) r

data FollowboxEvent n
	= Move (Event (n, n)) | LeftClick | SyncWaitMessage (Action Bool)
	| StoreRandoms (Action [Int]) | LoadRandoms (Event [Int])
	| StoreJsons (Action [Object]) | LoadJsons (Event [Object])
	| StoreRateLimitReset (Action (Maybe UTCTime))
	| LoadRateLimitReset (Event (Maybe UTCTime))
	| GetCurrentTime (Event UTCTime) | GetCurrentTimeZone (Event TimeZone)
	| CalcTextExtents
		(Bidirectional (FontName, FontSize, String) (XGlyphInfo n))
	| HttpGet Uri (Event ([Header], LBS.ByteString)) | Browse (Action Uri)
	| SleepUntil UTCTime | RaiseError (Action String)
	deriving (Show, Eq, Ord)

data XGlyphInfo n = XGlyphInfo {
	xGlyphInfoWidth :: n, xGlyphInfoHeight :: n,
	xGlyphInfoX :: n, xGlyphInfoY :: n,
	xGlyphInfoXOff :: n, xGlyphInfoYOff :: n } deriving (Show, Eq, Ord)

type FontName = String
type FontSize = Double
type Uri = String

isHttpGet :: FollowboxEvent n -> Bool
isHttpGet = \case (HttpGet _ _) -> True; _ -> False

isSleepUntil :: FollowboxEvent n -> Bool
isSleepUntil = \case (SleepUntil _) -> True; _ -> False

move :: (Show n, Ord n) => ReactF s n (n, n)
move = ex (Move Request) \evs ->
	case S.elems $ S.filter (== Move Request) evs of
		[Move (Occurred p)] -> p; es -> err es evs

leftClick :: (Show n, Ord n) => ReactF s n ()
leftClick = ex LeftClick \evs -> case S.elems $ S.filter (== LeftClick) evs of
	[LeftClick] -> (); es -> err es evs

syncWaitMessage :: (Show n, Ord n) => Bool -> ReactF s n ()
syncWaitMessage b = ex (SyncWaitMessage $ Cause b) \evs ->
	case S.elems $ S.filter (== SyncWaitMessage Response) evs of
		[SyncWaitMessage Response] -> (); es -> err es evs

storeRandoms :: (Show n, Ord n) => [Int] -> ReactF s n ()
storeRandoms rs = ex (StoreRandoms $ Cause rs) \evs ->
	case S.elems $ S.filter (== StoreRandoms Response) evs of
		[StoreRandoms Response] -> (); es -> err es evs

loadRandoms :: (Show n, Ord n) => ReactF s n [Int]
loadRandoms =  ex (LoadRandoms Request) \evs ->
	case S.elems $ S.filter (== LoadRandoms Request) evs of
		[LoadRandoms (Occurred rs)] -> rs; es -> err es evs

storeJsons :: (Show n, Ord n) => [Object] -> ReactF s n ()
storeJsons os =  ex (StoreJsons $ Cause os) \evs ->
	case S.elems $ S.filter (== StoreJsons Response) evs of
		[StoreJsons Response] -> (); es -> err es evs

loadJsons :: (Show n, Ord n) => ReactF s n [Object]
loadJsons = ex (LoadJsons Request) \evs ->
	case S.elems $ S.filter (== LoadJsons Request) evs of
		[LoadJsons (Occurred os)] -> os; es -> err es evs

storeRateLimitReset :: (Show n, Ord n) => Maybe UTCTime -> ReactF s n ()
storeRateLimitReset t = ex (StoreRateLimitReset $ Cause t) \evs ->
	case S.elems $ S.filter (== StoreRateLimitReset Response) evs of
		[StoreRateLimitReset Response] -> (); es -> err es evs

loadRateLimitReset :: (Show n, Ord n) => ReactF s n (Maybe UTCTime)
loadRateLimitReset =  ex (LoadRateLimitReset Request) \evs ->
	case S.elems $ S.filter (== LoadRateLimitReset Request) evs of
		[LoadRateLimitReset (Occurred t)] -> t; es -> err es evs

getCurrentTime :: (Show n, Ord n) => ReactF s n UTCTime
getCurrentTime = ex (GetCurrentTime Request) \evs ->
	case S.elems $ S.filter (== GetCurrentTime Request) evs of
		[GetCurrentTime (Occurred t)] -> t; es -> err es evs

getCurrentTimeZone :: (Show n, Ord n) => ReactF s n TimeZone
getCurrentTimeZone = ex (GetCurrentTimeZone Request) \evs ->
	case S.elems $ S.filter (== GetCurrentTimeZone Request) evs of
		[GetCurrentTimeZone (Occurred tz)] -> tz; es -> err es evs

calcTextExtents :: (Show n, Ord n) =>
	FontName -> FontSize -> String -> ReactF s n (XGlyphInfo n)
calcTextExtents fn fs str = ex (CalcTextExtents $ Action (fn, fs, str)) \evs ->
	case S.elems $ S.filter (== CalcTextExtents Communication) evs of 
		[CalcTextExtents (Event xgi)] -> xgi; es -> err es evs

httpGet :: (Show n, Ord n) => Uri -> ReactF s n ([Header], LBS.ByteString)
httpGet u = ex (HttpGet u Request) \evs ->
	case S.elems $ S.filter (== HttpGet u Request) evs of
		[HttpGet _ (Occurred bs)] -> bs; es -> err es evs

browse :: (Show n, Ord n) => Uri -> ReactF s n ()
browse u = ex (Browse $ Cause u) \evs ->
	case S.elems $ S.filter (== Browse Response) evs of
		[Browse Response] -> (); es -> err es evs

sleepUntil :: (Show n, Ord n) => UTCTime -> ReactF s n ()
sleepUntil t = ex (SleepUntil t) \evs ->
	case S.elems $ S.filter (== SleepUntil t) evs of
		[SleepUntil _] -> (); es -> err es evs

raiseError :: (Show n, Ord n) => String -> ReactF s n ()
raiseError em = ex (RaiseError $ Cause em) \evs ->
	case S.elems $ S.filter (== RaiseError Response) evs of
		[RaiseError Response] -> (); es -> err es evs

ex :: e -> (EvOccs e -> a) -> React s e a
ex e p = p <$> exper (S.singleton e)

err :: (HasCallStack, Show e) => [e] -> EvOccs e -> a
err es evs = error $ "never occur: " ++ show es ++ " : " ++ show evs
