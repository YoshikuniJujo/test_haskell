{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox where

import System.Random

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS

import Signal
import React
import Event
import AesonObject

import Check.Followbox.GetUsers

data FollowboxEvent
	= Http Uri (Event LBS.ByteString)
	| StoreRandoms (Action [Int]) | LoadRandoms (Event [Int])
	| StoreJsons (Action [Object]) | LoadJsons (Event [Object])
	| Prod
	deriving (Show, Eq, Ord)

type ReactF s r = React s FollowboxEvent r
type SigF s a r = Sig s FollowboxEvent a r

type Uri = String

httpGet :: Uri -> ReactF s LBS.ByteString
httpGet u = pick <$> exper (S.singleton $ Http u Request)
	where pick evs = case S.elems $ S.filter (== Http u Request) evs of
		[Http _ (Occurred bs)] -> bs
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

storeRandoms :: [Int] -> ReactF s ()
storeRandoms rs = pick <$> exper (S.singleton $ StoreRandoms (Cause rs))
	where pick evs = case S.elems $ S.filter (== StoreRandoms (Cause rs)) evs of
		[StoreRandoms Response] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

loadRandoms :: ReactF s [Int]
loadRandoms = pick <$> exper (S.singleton $ LoadRandoms Request)
	where pick evs = case S.elems $ S.filter (== LoadRandoms Request) evs of
		[LoadRandoms (Occurred rs)] -> rs
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

storeJsons :: [Object] -> ReactF s ()
storeJsons os = pick <$> exper (S.singleton $ StoreJsons (Cause os))
	where pick evs = case S.elems $ S.filter (== StoreJsons (Cause os)) evs of
		[StoreJsons Response] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

loadJsons :: ReactF s [Object]
loadJsons = pick <$> exper (S.singleton $ LoadJsons Request)
	where pick evs = case S.elems $ S.filter (== LoadJsons Request) evs of
		[LoadJsons (Occurred os)] -> os
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

prod :: ReactF s ()
prod = pick <$> exper (S.singleton Prod)
	where pick evs = case S.elems $ S.filter (== Prod) evs of
		[Prod] -> ()
		es -> error $ "never occur:i " ++ show es ++ " : " ++ show evs

getUsersJson :: Int -> ReactF s (Either String [Object])
getUsersJson s = decodeUsers <$> httpGet (apiUsers s)

getUser1 :: ReactF s (Maybe Object)
getUser1 = do
	oa <- loadJsons
	case oa of
		[] -> loadRandoms >>= \case
			r : rs -> do
				storeRandoms rs
				getUsersJson r >>= \case
					Right (o : os) ->
						Just o <$ storeJsons (take 4 os)
					Right [] -> error "no GitHub users"
					Left s -> error s
			[] -> error "no random numbers"
		o : os -> Just o <$ storeJsons os

apiUsers :: Int -> Uri
apiUsers s = "https://api.github.com/users?since=" ++ show s

tryUsers :: SigF s (Maybe Object) ()
tryUsers = waitFor (storeRandoms (randomRs (0, 499) (mkStdGen 8))) >> tu
	where
	tu = do	u <- waitFor getUser1
		emit u
		waitFor prod
		tu
