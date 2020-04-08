{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handlers where

import Control.Monad.State
import Data.Type.Set
import Data.String
import Data.UnionSet

import qualified Data.ByteString as BS
import qualified Network.HTTP.Simple as HTTP

import Trials.Followbox.Event
import Trials.Followbox.BasicAuth
import Trials.Followbox.Aeson
import MonadicFrp

handleHttpGet :: Maybe (BS.ByteString, FilePath) -> EvReqs (Singleton HttpGet) -> IO (EvOccs (Singleton HttpGet))
handleHttpGet mba reqs = case prj reqs of
	Just (HttpGetReq u) -> do
		r <- hg . setUserAgent "Yoshio" $ fromString u
		print $ HTTP.getResponseHeader "X-RateLimit-Remaining" r
		pure . singleton $ OccHttpGet u (HTTP.getResponseHeaders r) (HTTP.getResponseBody r)
	Nothing -> undefined
	where
	hg = case mba of
		Just (nm, tkn) -> httpBasicAuth nm tkn
		Nothing -> HTTP.httpLBS

setUserAgent :: BS.ByteString -> HTTP.Request -> HTTP.Request
setUserAgent ua = HTTP.setRequestHeader "User-Agent" [ua]

handleStoreJsons :: Monad m => EvReqs (Singleton StoreJsons) -> StateT [Object] m (EvOccs (Singleton StoreJsons))
handleStoreJsons reqs = case prj reqs of
	Just (StoreJsons os) -> singleton (OccStoreJsons os) <$ put os
	Nothing -> undefined

handleLoadJsons :: Monad m => EvReqs (Singleton LoadJsons) -> StateT [Object] m (EvOccs (Singleton LoadJsons))
handleLoadJsons reqs = case prj reqs of
	Just LoadJsonsReq -> singleton . OccLoadJsons <$> get
	Nothing -> undefined

handle :: Maybe (BS.ByteString, FilePath) -> EvReqs FollowboxEv -> StateT [Object] IO (EvOccs FollowboxEv)
handle mba reqs = do
	hg <- maybe (pure Nothing) ((Just <$>) . liftIO . handleHttpGet mba) $ collapse reqs
	sj <- maybe (pure Nothing) ((Just <$>) . handleStoreJsons) $ collapse reqs
	lj <- maybe (pure Nothing) ((Just <$>) . handleLoadJsons) $ collapse reqs
	case (hg `merge'` sj :: Maybe (EvOccs (HttpGet :- StoreJsons :- 'Nil))) `merge'` lj of
		Just occ -> pure occ
		Nothing -> handle mba reqs
