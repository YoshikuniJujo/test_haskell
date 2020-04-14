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
import Trials.Followbox.XGlyphInfo
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

getLoginNameQuit :: SigF View (Either T.Text (Maybe ()))
getLoginNameQuit = loginNameToView <$%> (repeat (adjust leftClick >> getLoginName) `until` checkQuit)

loginNameToView :: T.Text -> View
loginNameToView nm = [Text blue 24 (100, 100) nm]

loginNameNToView :: Int -> T.Text -> View1
loginNameNToView n nm = Text blue 24 (100, 100 + fromIntegral n * 50) nm

getLoginNameNQuit :: SigF View (Either View (Maybe ()))
getLoginNameNQuit = (repeat (adjust leftClick >> arrangeLoginNameN 3) `until` checkQuit)

arrangeLoginNameN :: Integer -> ReactF View
arrangeLoginNameN n = (concat . (fst <$>)) <$> viewLoginName `mapM` [0 .. n - 1]

createLoginName :: Color -> Double -> Position -> T.Text -> Position -> (View, Rect)
createLoginName clr fs p t (x', y') = (
	Text clr fs p t : createX 4 (round fs `div` 2) (x' + round fs `div` 2, y' + round fs * 3 `div` 8),
	Rect	(x' + round fs `div` 2, y' + round fs * 3 `div` 8)
		(x' + round fs, y' + round fs * 5 `div` 8) )

createX :: Integer -> Integer -> Position -> View
createX lw sz (x, y) = [
	Line white lw (x, y) (x + sz, y + sz),
	Line white lw (x + sz, y) (x, y + sz) ]

data Rect = Rect { upperLeft :: Position, bottomRight :: Position }
	deriving Show

viewLoginName :: Integer -> ReactF (View, Rect)
viewLoginName n = createLoginName1 n =<< getLoginName

createLoginName1 :: Integer -> T.Text -> ReactF (View, Rect)
createLoginName1 n t = do
	XGlyphInfo {
		xGlyphInfoWidth = w,
		xGlyphInfoX = x,
		xGlyphInfoY = y } <- adjust $ calcTextExtents "sans" 36 t
	pure $ createLoginName blue 36 (100, 100 + n * 72) t (100 - x + w, 100 + n * 72 - y)

-- clickOnRect :: Rect -> ReactF ()
-- clickOnRect

mousePosition :: SigF Position ()
mousePosition = repeat $ adjust move
