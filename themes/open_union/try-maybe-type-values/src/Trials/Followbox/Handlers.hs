{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handlers where

import Data.Type.Set
import Data.String
import Data.UnionSet

import qualified Data.ByteString as BS
import qualified Network.HTTP.Simple as HTTP

import Trials.Followbox.Event
import Trials.Followbox.BasicAuth
import MonadicFrp

handle :: Maybe (BS.ByteString, FilePath) -> EvReqs (Singleton HttpGet) -> IO (EvOccs (Singleton HttpGet))
handle mba reqs = case prj reqs of
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
