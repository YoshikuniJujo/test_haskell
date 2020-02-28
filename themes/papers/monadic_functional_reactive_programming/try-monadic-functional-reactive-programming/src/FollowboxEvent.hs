{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FollowboxEvent (
	ReactF, FollowboxEvent(..), Uri, Object, Value(..), XGlyphInfo(..), decodeJson,
	move, leftClick, httpGet, browse,
	storeRandoms, loadRandoms, storeJsons, loadJsons,
	calcTextExtents, raiseError ) where

import Prelude hiding (map, repeat, until)

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS

import React
import Event
import AesonObject

import XGlyphInfo

data FollowboxEvent n
	= Move (Event (n, n)) | LeftClick
	| Http Uri (Event LBS.ByteString) | Browse (Action Uri)
	| StoreRandoms (Action [Int]) | LoadRandoms (Event [Int])
	| StoreJsons (Action [Object]) | LoadJsons (Event [Object])
	| CalcTextExtents
		(Bidirectional (FontName, FontSize, String) (XGlyphInfo n))
	| Error (Action String)
	deriving (Show, Eq, Ord)

type Uri = String
type FontName = String
type FontSize = Double

type ReactF s n r = React s (FollowboxEvent n) r

move :: (Show n, Ord n) => ReactF s n (n, n)
move = ex (Move Request) \evs ->
	case S.elems $ S.filter (== Move Request) evs of
		[Move (Occurred p)] -> p; es -> err es evs

leftClick :: (Show n, Ord n) => ReactF s n ()
leftClick = ex LeftClick \evs -> case S.elems $ S.filter (== LeftClick) evs of
	[LeftClick] -> (); es -> err es evs

httpGet :: (Show n, Ord n) => Uri -> ReactF s n LBS.ByteString
httpGet u = ex (Http u Request) \evs ->
	case S.elems $ S.filter (== Http u Request) evs of
		[Http _ (Occurred bs)] -> bs; es -> err es evs

browse :: (Show n, Ord n) => Uri -> ReactF s n ()
browse u = ex (Browse $ Cause u) \evs ->
	case S.elems $ S.filter (== Browse (Cause u)) evs of
		[Browse _] -> (); es -> err es evs

storeRandoms :: (Show n, Ord n) => [Int] -> ReactF s n ()
storeRandoms rs = ex (StoreRandoms $ Cause rs) \evs ->
	case S.elems $ S.filter (== StoreRandoms (Cause rs)) evs of
		[StoreRandoms Response] -> (); es -> err es evs

loadRandoms :: (Show n, Ord n) => ReactF s n [Int]
loadRandoms =  ex (LoadRandoms Request) \evs ->
	case S.elems $ S.filter (== LoadRandoms Request) evs of
		[LoadRandoms (Occurred rs)] -> rs; es -> err es evs

storeJsons :: (Show n, Ord n) => [Object] -> ReactF s n ()
storeJsons os =  ex (StoreJsons $ Cause os) \evs ->
	case S.elems $ S.filter (== StoreJsons (Cause os)) evs of
		[StoreJsons Response] -> (); es -> err es evs

loadJsons :: (Show n, Ord n) => ReactF s n [Object]
loadJsons = ex (LoadJsons Request) \evs ->
	case S.elems $ S.filter (== LoadJsons Request) evs of
		[LoadJsons (Occurred os)] -> os; es -> err es evs

calcTextExtents :: (Show n, Ord n) => FontName -> FontSize -> String -> ReactF s n (XGlyphInfo n)
calcTextExtents fn fs str = ex (CalcTextExtents $ Action (fn, fs, str)) \evs ->
	case S.elems $ S.filter (== CalcTextExtents Communication) evs of 
		[CalcTextExtents (Event xo)] -> xo; es -> err es evs

raiseError :: (Show n, Ord n) => String -> ReactF s n ()
raiseError em = ex (Error $ Cause em) \evs ->
	case S.elems $ S.filter (== Error Response) evs of
		[Error Response] -> (); es -> err es evs

ex :: e -> (EvOccs e -> a) -> React s e a
ex e p = p <$> exper (S.singleton e)

err :: Show e => [e] -> EvOccs e -> a
err es evs = error $ "never occur: " ++ show es ++ " : " ++ show evs
