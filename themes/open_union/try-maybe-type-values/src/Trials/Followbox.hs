{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox where

import Control.Monad
import Data.Type.Set
import Data.Maybe

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Trials.Followbox.Event
import Trials.Followbox.Aeson
import MonadicFrp

getUsersJson :: React (Singleton HttpGet) (Either String [Object])
getUsersJson = decodeJson . snd <$> httpGet "https://api.github.com/users"

getUser1 :: ReactF (Maybe Object)
getUser1 = adjust loadJsons >>= \case
	[] -> adjust getUsersJson >>= \case
		Right (o : os) -> Just o <$ adjust (storeJsons $ take 8 os)
		Right [] -> adjust (raiseError NotJson "Empty JSON") >>= \case
			Continue -> getUser1
			Terminate -> pure Nothing
		Left em -> adjust (raiseError NotJson em) >>= \case
			Continue -> getUser1
			Terminate -> pure Nothing
	o : os -> Just o <$ adjust (storeJsons os)

getUserN :: Int -> ReactF [Object]
getUserN n = catMaybes <$> n `replicateM` getUser1

leftClickUserN :: Int -> ReactF [Either String T.Text]
leftClickUserN n = adjust leftClick >> (loginNameFromObject <$>) <$> getUserN n

loginNameFromObject :: Object -> Either String T.Text
loginNameFromObject o = case HM.lookup "login" o of
	Just (String li) -> Right li; _ -> Left "no login name"
