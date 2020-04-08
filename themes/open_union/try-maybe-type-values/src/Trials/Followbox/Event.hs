{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Event where

import Data.Type.Set
import Data.Bool
import Network.HTTP.Simple (Header)

import qualified Data.ByteString.Lazy as LBS

import MonadicFrp

data HttpGet = HttpGetReq Uri deriving (Show, Eq, Ord)
type Uri = String

numbered [t| HttpGet |]
instance Request HttpGet where
	data Occurred HttpGet = OccHttpGet Uri [Header] LBS.ByteString
		deriving Show

httpGet :: Uri -> React (Singleton HttpGet) (Maybe ([Header], LBS.ByteString))
httpGet u = await (HttpGetReq u)
	\(OccHttpGet u' hs c) -> bool Nothing (Just (hs, c)) $ u == u'
