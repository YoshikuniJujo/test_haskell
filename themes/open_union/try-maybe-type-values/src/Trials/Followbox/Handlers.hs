{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handlers where

import Data.Type.Set
import Data.String
import Data.UnionSet

import qualified Data.ByteString as BS
import qualified Network.HTTP.Simple as HTTP

import Trials.Followbox.Event
import MonadicFrp

handle :: EvReqs (Singleton HttpGet) -> IO (EvOccs (Singleton HttpGet))
handle reqs = case prj reqs of
	Just (HttpGetReq u) -> do
		r <- HTTP.httpLBS . setUserAgent "Yoshio" $ fromString u
		print $ HTTP.getResponseHeader "X-RateLimit-Remaining" r
		pure . singleton $ OccHttpGet u (HTTP.getResponseHeaders r) (HTTP.getResponseBody r)
	Nothing -> undefined

setUserAgent :: BS.ByteString -> HTTP.Request -> HTTP.Request
setUserAgent ua = HTTP.setRequestHeader "User-Agent" [ua]
