{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox where

import Data.Type.Set
import Data.Aeson
import Network.HTTP.Simple

import qualified Data.ByteString.Lazy as LBS

import Trials.Followbox.Event
import MonadicFrp

httpGet' :: Uri -> React (Singleton HttpGet) ([Header], LBS.ByteString)
httpGet' u = maybe (httpGet' u) pure =<< httpGet u

getUsersJson :: React (Singleton HttpGet) (Either String [Object])
getUsersJson = eitherDecode . snd <$> httpGet' "https://api.github.com/users"
