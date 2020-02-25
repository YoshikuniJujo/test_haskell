{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad.State
import Data.String
import Network.HTTP.Simple

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS

import Signal
import React hiding (first)
import Event

import Followbox
import AesonObject
import BasicAuth

main :: IO ()
main = interpretSig handle (liftIO . print) tryUsers `runStateT` ([], []) >>= print

type FollowboxIO = StateT ([Int], [Object]) IO

getRandoms :: FollowboxIO [Int]
getRandoms = fst <$> get

putRandoms :: [Int] -> FollowboxIO ()
putRandoms rs = modify (const rs `first`)

getObjects :: FollowboxIO [Object]
getObjects = snd <$> get

putObjects :: [Object] -> FollowboxIO ()
putObjects os = modify (const os `second`)

http :: String -> IO LBS.ByteString
http u = do
	rsp <- httpBasicAuth
		"YoshikuniJujo" "github_token.txt"
		(setRequestHeader "User-Agent" ["Yoshio"] (fromString u))
	print $ getResponseHeader "X-RateLimit-Remaining" rsp
	pure $ getResponseBody rsp

handle :: EvReqs FollowboxEvent -> FollowboxIO (EvOccs FollowboxEvent)
handle evs
	| Prod `S.member` evs = liftIO getLine >> pure (S.singleton Prod)
	| Just (Http uri _) <- S.lookupMin $ S.filter isHttp evs = liftIO $ S.singleton . Http uri . Occurred <$> http uri
	| Just (StoreRandoms (Cause rs)) <- S.lookupMin $ S.filter (== StoreRandoms Response) evs =
		S.singleton (StoreRandoms Response) <$ putRandoms rs
	| Just (LoadRandoms Request) <- S.lookupMin $ S.filter (== LoadRandoms Request) evs =
		S.singleton . LoadRandoms . Occurred <$> getRandoms
	| Just (StoreJsons (Cause os)) <- S.lookupMin $ S.filter (== StoreJsons Response) evs =
		S.singleton (StoreJsons Response) <$ putObjects os
	| Just (LoadJsons Request) <- S.lookupMin $ S.filter (== LoadJsons Request) evs =
		S.singleton . LoadJsons . Occurred <$> getObjects
	| otherwise = error "bad"

isHttp :: FollowboxEvent -> Bool
isHttp (Http _ _) = True
isHttp _ = False
