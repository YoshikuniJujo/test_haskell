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
handleHttpGet mba reqs = do
	r <- hg . setUserAgent "Yoshio" $ fromString u
	print $ HTTP.getResponseHeader "X-RateLimit-Remaining" r
	pure . singleton $ OccHttpGet u (HTTP.getResponseHeaders r) (HTTP.getResponseBody r)
	where
	hg = case mba of
		Just (nm, tkn) -> httpBasicAuth nm tkn
		Nothing -> HTTP.httpLBS
	HttpGetReq u = extract reqs

setUserAgent :: BS.ByteString -> HTTP.Request -> HTTP.Request
setUserAgent ua = HTTP.setRequestHeader "User-Agent" [ua]

handleStoreJsons :: Monad m => EvReqs (Singleton StoreJsons) -> StateT [Object] m (EvOccs (Singleton StoreJsons))
handleStoreJsons reqs = singleton (OccStoreJsons os) <$ put os
	where StoreJsons os = extract reqs

handleLoadJsons :: Monad m => EvReqs (Singleton LoadJsons) -> StateT [Object] m (EvOccs (Singleton LoadJsons))
handleLoadJsons _reqs = singleton . OccLoadJsons <$> get

handleLeftClick :: EvReqs (Singleton LeftClick) -> IO (EvOccs (Singleton LeftClick))
handleLeftClick _reqs = getLine >> pure (singleton OccLeftClick)

handleRaiseError :: EvReqs (Singleton RaiseError) -> IO (EvOccs (Singleton RaiseError))
handleRaiseError reqs = do
	putStrLn $ "ERROR: " ++ em
	case e of
		NotJson -> pure . singleton $ OccRaiseError e Terminate
	where RaiseError e em = extract reqs

handle :: Maybe (BS.ByteString, FilePath) -> EvReqs FollowboxEv -> StateT [Object] IO (EvOccs FollowboxEv)
handle mba reqs = do
	lc <- maybe (pure Nothing) ((Just <$>) . liftIO . handleLeftClick) $ collapse reqs
	hg <- maybe (pure Nothing) ((Just <$>) . liftIO . handleHttpGet mba) $ collapse reqs
	sj <- maybe (pure Nothing) ((Just <$>) . handleStoreJsons) $ collapse reqs
	lj <- maybe (pure Nothing) ((Just <$>) . handleLoadJsons) $ collapse reqs
	re <- maybe (pure Nothing) ((Just <$>) . liftIO . handleRaiseError) $ collapse reqs
	case (((re `merge'` lc :: Maybe (EvOccs (RaiseError :- LeftClick :- 'Nil))) `merge'` hg :: Maybe (EvOccs (RaiseError :- LeftClick :- HttpGet :- 'Nil))) `merge'` sj :: Maybe (EvOccs (RaiseError :- LeftClick :- HttpGet :- StoreJsons :- 'Nil))) `merge'` lj of
		Just occ -> pure occ
		Nothing -> handle mba reqs
