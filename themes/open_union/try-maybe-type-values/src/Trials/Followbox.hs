{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox (followbox) where

import Prelude hiding (until, repeat)

import Control.Monad (forever, replicateM)
import Data.Type.Flip ((<$%>), (<*%>), ftraverse)
import Data.Or (Or(..))
import Data.Time (utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Graphics.X11.Xrender (XGlyphInfo(..))

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Codec.Picture hiding (Image)
import Codec.Picture.Extra

import MonadicFrp (adjust, first, emit, waitFor, find, repeat, until, indexBy)
import Trials.Followbox.Event (
	SigF, ReactF, move, leftClick, clearJsons, storeJsons, loadJsons,
	httpGet, calcTextExtents, getTimeZone, browse,
	beginSleep, checkBeginSleep, endSleep, checkQuit,
	Error(..), ErrorResult(..), raiseError, catchError )
-- import Trials.Followbox.View (View, View1(..), Color, white, blue)
import Trials.Followbox.View (View, View1(..), white, blue)
import Trials.Followbox.Random (getRandomR)
-- import Trials.Followbox.TypeSynonym (Position, Avatar, ErrorMessage, FontName, FontSize, LineWidth)
import Trials.Followbox.TypeSynonym (Position, Avatar, ErrorMessage, FontName, FontSize)

---------------------------------------------------------------------------

followbox :: SigF View ()
followbox = () <$ fieldWithResetTime 3 `until` checkQuit `until` checkTerminate
	where checkTerminate = catchError >>=
		\case Continue -> checkTerminate; Terminate -> pure ()

fieldWithResetTime :: Integer -> SigF View ()
fieldWithResetTime n = (<>) <$%> field n <*%> resetTime

resetTime :: SigF View ()
resetTime = forever $ (emit [] >>) do
	emit =<< waitFor do
		(t, tz) <- (,) <$> adjust checkBeginSleep <*> adjust getTimeZone
		pure [Text white "sans" 30 (100, 500) . T.pack
			$ "Wait until " <> show (utcToLocalTime tz t)]
	waitFor $ adjust endSleep

field :: Integer -> SigF View ()
field n = do
	((vn, rn), (vr, rr)) <- waitFor $ (,)
		<$> linkText (500, 80) "Next" <*> linkText (600, 80) "Refresh"
	let	frame = title : vn <> vr
		clear = emit frame
	clear >> forever do
		us <- fromIntegral n `replicateM` waitFor getUser
		(frame <>) <$%> users us `until` rn `first` rr >>= \case
			Left (_, L _) -> pure ()
			Left (_, LR _ _) -> pure ()
			Left (_, R _) -> clear >> waitFor (adjust clearJsons)
			Right _ -> error "never occur"
	where
	title = Text white "sans" 36 (50, 80) "Who to follow"
	linkText p t = clickableText p <$> withTextExtents "sans" 30 t

users :: [(Avatar, T.Text, T.Text)] -> SigF View ()
users us = concat <$%> uncurry user1 `ftraverse` zip [0 ..] us

user1 :: Integer -> (Avatar, T.Text, T.Text) -> SigF View ()
user1 n (a, nm, u) = do
	((v, l), (v', x)) <- waitFor $ name n nm
	emit $ Image (100, 120 + 120 * n) a : (v <> v')
	() <$ waitFor (forever $ l >> adjust (browse u)) `until` x
	user1 n =<< waitFor getUser

data Rect = Rect { upperLeft :: Position, bottomRight :: Position }
	deriving Show

type Clickable = (View, ReactF ())

name :: Integer -> T.Text -> ReactF (Clickable, Clickable)
name n t = (<$> withTextExtents "sans" fs t) \wte ->
	(clickableText p wte, clickableX xs $ positionX p wte)
	where p = (210, 180 + n * 120); fs = 36; xs = round fs `div` 2

type WithTextExtents = (FontName, FontSize, T.Text, XGlyphInfo)

withTextExtents :: FontName -> FontSize -> T.Text -> ReactF WithTextExtents
withTextExtents fn fs t = (fn, fs, t,) <$> adjust (calcTextExtents fn fs t)

clickableText :: Position -> WithTextExtents -> Clickable
clickableText (x, y) (fn, fs, t, xg) = (
	[Text blue fn fs (x, y) t], 
	clickOnRect $ Rect (x - gx, y - gy) (x - gx + gw, y - gy + gh) )
	where [gx, gy, gw, gh] = fromIntegral . ($ xg) <$> [
		xglyphinfo_x, xglyphinfo_y,
		xglyphinfo_width, xglyphinfo_height ]

clickableX :: Integer -> (Integer, Integer) -> Clickable
clickableX sz (x0, y0) = (
	[Line white 4 (x0, y0) (x1, y1), Line white 4 (x1, y0) (x0, y1)],
	clickOnRect $ Rect (x0, y0) (x1, y1) )
	where [x1, y1] = (+ sz) <$> [x0, y0]

positionX :: Position -> WithTextExtents -> Position
positionX (x, y) (_, fs, _, xg) = (
	x - fromXg xglyphinfo_x + fromXg xglyphinfo_width + round fs `div` 2,
	y - fromXg xglyphinfo_y + round fs * 3 `div` 8 )
	where fromXg = fromIntegral . ($ xg)

clickOnRect :: Rect -> ReactF ()
clickOnRect (Rect (l, t) (r, b)) =
	() <$ find isInside (mousePosition `indexBy` repeat leftClick)
	where
	mousePosition :: SigF Position ()
	mousePosition = repeat $ adjust move
	isInside (x, y) = l <= x && x <= r && t <= y && y <= b

getUser :: ReactF (Avatar, T.Text, T.Text)
getUser = makeUser <$> getObject1 >>= \case
	Left (e, em) -> adjust (raiseError e em) >> getUser
	Right (au, l, u) -> getAvatar au >>= \case
		Left (e, em) -> adjust (raiseError e em) >> getUser
		Right a -> pure (a, l, u)
	where
	makeUser o = (,,)
		<$> getField o "avatar_url" NoAvatarAddress "No Avatar Address"
		<*> getField o "login" NoLoginName "No Login Name"
		<*> getField o "html_url" NoHtmlUrl "No HTML URL"
	getField o k e em = case HM.lookup k o of
		Just (String li) -> Right li; _ -> Left (e, em)

getAvatar :: T.Text -> ReactF (Either (Error, ErrorMessage) Avatar)
getAvatar url = (<$> adjust (httpGet url)) . (. bsToImage . snd)
	$ either (Left . (NoAvatar ,)) (Right . scaleBilinear 80 80)
	where bsToImage lbs = convertRGBA8 <$> decodeImage (LBS.toStrict lbs)

getObject1 :: ReactF Object
getObject1 = adjust loadJsons >>= \case
	[] -> getObjects >>= \case
		Right (o : os) -> o <$ adjust (storeJsons $ take 8 os)
		Right [] -> do
			adjust $ raiseError EmptyJson "Empty JSON"
			getObject1
		Left em -> adjust (raiseError NotJson em) >> getObject1
	o : os -> o <$ adjust (storeJsons os)

getObjects :: ReactF (Either String [Object])
getObjects = do
	n <- adjust $ getRandomR (0, 2 ^ (27 :: Int))
	(h, b) <- adjust . httpGet $ apiUsers n
	case (lookupRateLimitRemaining h, lookupRateLimitReset h) of
		(Just rmn, _) | rmn > (0 :: Int) -> pure $ eitherDecode b
		(Just _, Just t) ->
			adjust (beginSleep t) >> adjust endSleep >> getObjects
		(Just _, Nothing) -> error "No X-RateLimit-Reset"
		(Nothing, _) -> error "No X-RateLimit-Remaining header"
	where
	lookupRateLimitRemaining =
		(read . BSC.unpack <$>) . lookup "X-RateLimit-Remaining"
	lookupRateLimitReset =
		(posixSecondsToUTCTime . fromInteger . read . BSC.unpack <$>)
			. lookup "X-RateLimit-Reset"
	apiUsers = ("https://api.github.com/users?since=" <>) . T.pack . show @Int
