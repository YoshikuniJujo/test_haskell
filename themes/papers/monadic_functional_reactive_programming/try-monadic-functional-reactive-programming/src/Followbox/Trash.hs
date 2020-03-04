{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.Trash where

import System.Random

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS

import Signal
import React
import Event
import Followbox.AesonObject

import Debug.Trace

type Uri = String

data FollowboxEvent 
	= Http Uri (Event LBS.ByteString)
	| ServeUser
	| NeedUser
	| Prod
	deriving (Show, Eq, Ord)

data ServeNeed = Serve | Need deriving (Show, Eq, Ord)

type ReactF s r = React s FollowboxEvent r
type SigF s a r = Sig s FollowboxEvent a r

apiUsers :: Int -> Uri
apiUsers s = "https://api.github.com/users?since=" ++ show s

getUsersByteString :: Int -> ReactF s LBS.ByteString
getUsersByteString s = pick <$> exper (S.singleton $ Http (apiUsers s) Request)
	where pick evs = case S.elems $ S.filter (== Http (apiUsers s) Request) evs of
		[Http _ (Occurred bs)] -> bs
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

getServeUser :: ReactF s ()
getServeUser = pick <$> exper (S.singleton ServeUser)
	where pick evs = case S.elems $ S.filter (== ServeUser) evs of
		[ServeUser] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

getNeedUser :: ReactF s ()
getNeedUser = pick <$> exper (S.singleton NeedUser)
	where pick evs = case S.elems $ S.filter (== NeedUser) evs of
		[NeedUser] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

getProd :: ReactF s ()
getProd = pick <$> exper (S.singleton Prod)
	where pick evs = case S.elems $ S.filter (== Prod) evs of
		[Prod] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

getUsersJsonReact :: Int -> ReactF s [Object]
getUsersJsonReact s = (\(Right r) -> r) . decodeJson <$> getUsersByteString s

getUsersJson :: [Int] -> SigF s Object ()
getUsersJson (s : ss) = do
	us <- waitFor $ getUsersJsonReact s
	go $ take 5 us
	where
	go [] = getUsersJson ss
	go (u : us) = do
		emit u
		waitFor getServeUser
		trace "getUsersJson: after emit" $ pure ()
		go us
getUsersJson [] = error "getUsersJson: an argument should not be empty"

prodUser :: SigF s Object ()
prodUser = getUsersJson (randomRs (0, 499) (mkStdGen 8)) `indexBy` loop
	where loop = emit () >> waitFor getProd >> waitFor getNeedUser >> loop
{-
prodUser = void $ always const <^> getUsersJson (randomRs (0, 499) (mkStdGen 8)) <^> loop
	where loop = emit () >> waitFor getProd >> waitFor getNeedUser >> loop
	-}
