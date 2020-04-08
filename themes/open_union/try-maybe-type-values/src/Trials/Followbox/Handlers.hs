{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Handlers where

import Data.Type.Set
import Data.String
import Data.UnionSet
import Network.HTTP.Simple

import Trials.Followbox.Event
import MonadicFrp

handle :: EvReqs (Singleton HttpGet) -> IO (EvOccs (Singleton HttpGet))
handle reqs = case prj reqs of
	Just (HttpGetReq u) -> (<$> httpLBS (fromString u)) \r ->
		singleton $ OccHttpGet u (getResponseHeaders r) (getResponseBody r)
	Nothing -> undefined
