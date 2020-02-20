{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Data.Set as S

import Signal
import React
import Event

import Followbox
import Check.Followbox.GetUsers

main :: IO ()
main = interpretSig handle print prodUser >>= print

handle :: EvReqs FollowboxEvent -> IO (EvOccs FollowboxEvent)
handle evs
	| Prod `S.member` evs = do
		putStrLn $ "handle evs: Prod " ++ show evs
		if S.filter isHttp evs == S.empty
			then getLine >> pure (S.singleton Prod)
			else handle1 . S.findMin $ S.filter isHttp evs
	| ServeUser `S.member` evs, NeedUser `S.member` evs = do
		putStrLn $ "handle evs: ServeUser and NeedUser " ++ show evs
		ht <- if S.filter isHttp evs == S.empty then pure S.empty else handle1 . S.findMin $ S.filter isHttp evs
		pure $ S.fromList [ServeUser, NeedUser] <> ht
	| otherwise = handle1 . S.findMin $ S.filter isHttp evs

handle1 :: FollowboxEvent -> IO (EvOccs FollowboxEvent)
handle1 (Http uri _) = S.singleton . Http uri . Occurred <$> do
	putStrLn "handle1: Http"
	getUsers
handle1 _ = error "never occur"

isHttp :: FollowboxEvent -> Bool
isHttp (Http _ _) = True
isHttp _ = False
