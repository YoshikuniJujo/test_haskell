{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox where

import Data.Type.Set
import Network.HTTP.Simple

import qualified Data.ByteString.Lazy as LBS

import Trials.Followbox.Event
import Trials.Followbox.Aeson
import MonadicFrp

httpGet' :: Uri -> React (Singleton HttpGet) ([Header], LBS.ByteString)
httpGet' u = maybe (httpGet' u) pure =<< httpGet u

getUsersJson :: React (Singleton HttpGet) (Either String [Object])
getUsersJson = decodeJson . snd <$> httpGet' "https://api.github.com/users"

storeJsons' :: [Object] -> React (Singleton StoreJsons) ()
storeJsons' os = result (storeJsons' os) (pure ()) =<< storeJsons os

getUser1 :: ReactF Object
getUser1 = adjust loadJsons >>= \case
	[] -> adjust getUsersJson >>= \case
		Right (o : os) -> o <$ adjust (storeJsons os)
		_ -> error "no github users"
	o : os -> o <$ adjust (storeJsons os)
