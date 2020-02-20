{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox where

import Control.Monad
import Data.Maybe

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A

import Signal
import React
import Event

import Check.Followbox.GetUsers

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

apiUsers :: Uri
apiUsers = "https://api.github.com/users"

getUsersByteString :: ReactF s LBS.ByteString
getUsersByteString = pick <$> exper (S.singleton $ Http apiUsers Request)
	where pick evs = case S.elems $ S.filter (== Http apiUsers Request) evs of
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

getUsersJsonReact :: ReactF s [A.Object]
getUsersJsonReact = (\(Right r) -> r) . decodeUsers <$> getUsersByteString

getUsersJson :: SigF s A.Object ()
getUsersJson = do
	us <- waitFor getUsersJsonReact
	go $ take 5 us
	where
	go [] = getUsersJson
	go (u : us) = do
		waitFor getServeUser
		emit u
		trace "getUsersJson: after emit" $ pure ()
		go us

prodUser :: SigF s A.Object ()
prodUser = void $ always const <^> getUsersJson <^> loop
	where
	loop = emit () >> waitFor getProd >> waitFor getNeedUser >> loop
