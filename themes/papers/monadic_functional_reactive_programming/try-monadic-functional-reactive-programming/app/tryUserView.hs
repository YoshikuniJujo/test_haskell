{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad.State
import Data.String
import System.Exit
import System.Process
import Network.HTTP.Simple

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS

import Signal
import React hiding (first)
import Event
import Field
import ButtonEvent

import Followbox
import AesonObject
import BasicAuth
import View

main :: IO ()
main = do
	f <- openField ("GitHubのユーザを表示するよ" :: String) [exposureMask, buttonPressMask]
	interpretSig (handle f) (liftIO . either error (view f)) userView `runStateT` ([], []) >>= print
	closeField f

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
	rsp <- httpBasicAuth "YoshikuniJujo" "github_token.txt"
		. setRequestHeader "User-Agent" ["Yoshio"] $ fromString u
	print $ getResponseHeader "X-RateLimit-Remaining" rsp
	pure $ getResponseBody rsp

handle :: Field -> EvReqs FollowboxEvent -> FollowboxIO (EvOccs FollowboxEvent)
handle f evs
	| Prod `S.member` evs = withNextEvent f $ handleEvent f evs
	| Just (Http uri _) <- S.lookupMin $ S.filter isHttp evs =
		liftIO $ S.singleton . Http uri . Occurred <$> http uri
	| Just (StoreRandoms (Cause rs)) <- S.lookupMin $ S.filter (== StoreRandoms Response) evs =
		S.singleton (StoreRandoms Response) <$ putRandoms rs
	| Just (LoadRandoms Request) <- S.lookupMin $ S.filter (== LoadRandoms Request) evs =
		S.singleton . LoadRandoms . Occurred <$> getRandoms
	| Just (StoreJsons (Cause os)) <- S.lookupMin $ S.filter (== StoreJsons Response) evs =
		S.singleton (StoreJsons Response) <$ putObjects os
	| Just (LoadJsons Request) <- S.lookupMin $ S.filter (== LoadJsons Request) evs =
		S.singleton . LoadJsons . Occurred <$> getObjects
	| Just (Browse (Cause uri)) <- S.lookupMin $ S.filter (== Browse Response) evs =
		S.singleton (Browse Response) <$ liftIO (putStrLn uri >> rawSystem "firefox" [uri])
	| otherwise = error "bad"

isHttp :: FollowboxEvent -> Bool
isHttp (Http _ _) = True
isHttp _ = False

handleEvent :: Field -> EvReqs FollowboxEvent -> Field.Event -> FollowboxIO (EvOccs FollowboxEvent)
handleEvent f evs = \case
	DestroyWindowEvent {} -> liftIO $ closeField f >> exitSuccess
	ExposeEvent {} -> liftIO (flushField f) >> handle f evs
	ev -> case buttonEvent ev of
		Just BtnEvent { buttonNumber = Button1 } -> pure $ S.singleton Prod
		Just BtnEvent { buttonNumber = Button3 } -> pure $ S.singleton RightClick
		Just _ -> handle f evs
		Nothing	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle f evs
			| otherwise -> liftIO (print ev) >> handle f evs
