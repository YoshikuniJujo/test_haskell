{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox where

import Prelude hiding (until, repeat)

import Control.Monad
import Data.Type.Flip
import Data.Type.Set
import Data.Or

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Trials.Followbox.Event
import Trials.Followbox.View
import Trials.Followbox.Aeson
-- import Trials.Followbox.XGlyphInfo
import MonadicFrp

getUsersJson :: React (Singleton HttpGet) (Either String [Object])
getUsersJson = decodeJson . snd <$> httpGet "https://api.github.com/users"

getUser1 :: ReactF Object
getUser1 = adjust loadJsons >>= \case
	[] -> adjust getUsersJson >>= \case
		Right (o : os) -> o <$ adjust (storeJsons $ take 8 os)
		Right [] -> adjust (raiseError NotJson "Empty JSON") >> getUser1
		Left em -> adjust (raiseError NotJson em) >> getUser1
	o : os -> o <$ adjust (storeJsons os)

getUserN :: Int -> ReactF [Object]
getUserN n = n `replicateM` getUser1

getLoginName :: ReactF T.Text
getLoginName = loginNameFromObject <$> getUser1 >>= \case
	Just ln -> pure ln
	Nothing -> adjust (raiseError NoLoginName "No Login Name") >> getLoginName

getLoginNameN :: Int -> ReactF [T.Text]
getLoginNameN n = n `replicateM` getLoginName

leftClickUserN :: Int -> ReactF [Maybe T.Text]
leftClickUserN n = adjust leftClick >> (loginNameFromObject <$>) <$> getUserN n

loginNameFromObject :: Object -> Maybe T.Text
loginNameFromObject o = case HM.lookup "login" o of
	Just (String li) -> Just li; _ -> Nothing

terminateOccur :: ReactF ()
terminateOccur = adjust catchError >>= \case
	Continue -> terminateOccur
	Terminate -> pure ()

getUser1UntilError :: ReactF (Or Object ())
getUser1UntilError = getUser1 `first` terminateOccur

getLoginNameNUntilError :: Int -> ReactF (Or [T.Text] ())
getLoginNameNUntilError n = getLoginNameN n `first` terminateOccur

getLoginNameQuit :: SigF View (Either T.Text (Maybe ()))
getLoginNameQuit = loginNameToView <$%> (repeat (adjust leftClick >> getLoginName) `until` checkQuit)

loginNameToView :: T.Text -> View
loginNameToView nm = [Text blue 24 (100, 100) nm]

getLoginNameWithN :: Int -> ReactF [(Int, T.Text)]
getLoginNameWithN n = zip [0 ..] <$> getLoginNameN n

loginNameNToView :: Int -> T.Text -> View1
loginNameNToView n nm = Text blue 24 (100, 100 + fromIntegral n * 50) nm

getLoginNameNQuit :: SigF View (Either View (Maybe ()))
getLoginNameNQuit = (repeat (adjust leftClick >> arrangeLoginNameN 3) `until` checkQuit)

arrangeVertically :: Color -> FontSize -> Position -> [T.Text] -> ReactF View
arrangeVertically _ _ _ [] = pure []
arrangeVertically clr fs p@(x, y) (t : ts) = do
--	XGlyphInfo { xGlyphInfoHeight = h } <- adjust $ calcTextExtents "sans" fs t
--	(Text clr fs p t :) <$> arrangeVertically clr fs (x, y + h * 3 `div` 2) ts
	(Text clr fs p t :) <$> arrangeVertically clr fs (x, y + round fs * 3 `div` 2) ts

arrangeLoginNameN :: Int -> ReactF View
arrangeLoginNameN n = arrangeVertically blue 24 (100, 100) =<<  getLoginNameN n
