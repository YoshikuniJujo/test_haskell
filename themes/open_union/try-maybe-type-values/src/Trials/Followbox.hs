{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox where

import Control.Monad
import Data.Type.Set

import Trials.Followbox.Event
import Trials.Followbox.Aeson
import MonadicFrp

getUsersJson :: React (Singleton HttpGet) (Either String [Object])
getUsersJson = decodeJson . snd <$> httpGet "https://api.github.com/users"

getUser1 :: ReactF Object
getUser1 = adjust loadJsons >>= \case
	[] -> adjust getUsersJson >>= \case
		Right (o : os) -> o <$ adjust (storeJsons $ take 8 os)
		_ -> error "no github users"
	o : os -> o <$ adjust (storeJsons os)

getUserN :: Int -> ReactF [Object]
getUserN n = n `replicateM` getUser1

leftClickUserN :: Int -> ReactF [Object]
leftClickUserN n = adjust leftClick >> getUserN n
