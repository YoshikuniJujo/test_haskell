{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox (followbox) where

import Prelude hiding (until, repeat)

import Control.Monad (forever, replicateM)
import Data.Type.Flip ((<$%>), (<*%>), ftraverse)
import Data.Or (Or(..))
import Data.Time (UTCTime, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Text.Read (readMaybe)
import Graphics.X11.Xrender (XGlyphInfo(..))
import Codec.Picture (convertRGBA8, decodeImage)
import Codec.Picture.Extra (scaleBilinear)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import MonadicFrp (adjust, first, emit, waitFor, find, repeat, until, indexBy)
import Trials.Followbox.Event (
	SigF, ReactF, move, leftClick, clearJsons, storeJsons, loadJsons,
	httpGet, calcTextExtents, getTimeZone, browse,
	beginSleep, checkBeginSleep, endSleep, checkQuit,
	Error(..), raiseError, checkTerminate )
import Trials.Followbox.View (View, View1(..), white, blue)
import Trials.Followbox.Random (getRandomR)
import Trials.Followbox.TypeSynonym (
	Position, Avatar, FontName, FontSize, ErrorMessage )

---------------------------------------------------------------------------

numOfUsers :: Integer
numOfUsers = 3

defaultFont :: FontName
defaultFont = "sans"

middleSize, largeSize :: FontSize
middleSize = 30; largeSize = 36

avatarSizeX, avatarSizeY :: Int
(avatarSizeX, avatarSizeY) = (80, 80)

titlePosition, nextPosition, refreshPosition, resetTimePosition :: Position
titlePosition = (50, 80); nextPosition = (500, 80)
refreshPosition = (600, 80); resetTimePosition = (100, 500)

avatarPosition, namePosition :: Integer -> Position
avatarPosition n = (100, 120 * (n + 1))
namePosition n = (210, 120 * (n + 1) + 60)

crossSize :: Integer
crossSize = round largeSize `div` 2

crossPosition :: Position -> WithTextExtents -> Position
crossPosition p wte = translate (nextToText p wte) wte (1 / 2, - 5 / 8)

crossMergin :: Integer
crossMergin = 4

---------------------------------------------------------------------------

followbox :: SigF View ()
followbox = () <$
	fieldWithResetTime numOfUsers `until` checkQuit `until` checkTerminate

fieldWithResetTime :: Integer -> SigF View ()
fieldWithResetTime n = (<>) <$%> field n <*%> resetTime

resetTime :: SigF View ()
resetTime = forever $ (emit [] >>) do
	emit =<< waitFor do
		(t, tz) <- (,) <$> adjust checkBeginSleep <*> adjust getTimeZone
		pure [twhite middleSize resetTimePosition . T.pack
			$ "Wait until " <> show (utcToLocalTime tz t)]
	waitFor $ adjust endSleep

field :: Integer -> SigF View ()
field n = do
	(nxt, rfs) <- waitFor $ (,)
		<$> link nextPosition "Next"
		<*> link refreshPosition "Refresh"
	let	frame = title : view nxt <> view rfs
		clear = emit frame
	clear >> forever do
		us <- waitFor $ fromIntegral n `replicateM` getUser
		(frame <>) <$%>
			users us `until` click nxt `first` click rfs >>= \case
				Left (_, L _) -> pure ()
				Left (_, LR _ _) -> pure ()
				Left (_, R _) ->
					clear >> waitFor (adjust clearJsons)
				Right _ -> error "never occur"
	where
	title = twhite largeSize titlePosition "Who to follow"
	link p t = clickableText p <$> withTextExtents defaultFont middleSize t

twhite :: FontSize -> Position -> T.Text -> View1
twhite = Text white defaultFont

users :: [(Avatar, T.Text, T.Text)] -> SigF View ()
users us = concat <$%> uncurry user1 `ftraverse` zip [0 ..] us

user1 :: Integer -> (Avatar, T.Text, T.Text) -> SigF View ()
user1 n (a, ln, u) = do
	(nm, cr) <- waitFor $ name n ln
	emit $ Image (avatarPosition n) a : view nm <> view cr
	() <$ waitFor (forever $ click nm >> adjust (browse u)) `until` click cr
	user1 n =<< waitFor getUser

name :: Integer -> T.Text -> ReactF (Clickable, Clickable)
name n t = (<$> withTextExtents defaultFont largeSize t) \wte ->
	(clickableText p wte, cross crossSize $ crossPosition p wte)
	where p = namePosition n

cross :: Integer -> (Integer, Integer) -> Clickable
cross sz (x0, y0) = (
	[Line white 4 (x0, y0) (x1, y1), Line white 4 (x1, y0) (x0, y1)],
	clickOnRect $ Rect
		(x0 - crossMergin, y0 - crossMergin)
		(x1 + crossMergin, y1 + crossMergin) )
	where [x1, y1] = (+ sz) <$> [x0, y0]

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
getAvatar url = (<$> adjust (httpGet url)) . (. bsToImage . snd) $ either
	(Left . (NoAvatar ,)) (Right . scaleBilinear avatarSizeX avatarSizeY)
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
	(h, b) <- adjust . httpGet $ api n
	case (rlRmnng h, rlRst h) of
		(Just rmn, _) | rmn > (0 :: Int) -> pure $ eitherDecode b
		(Just _, Just t) ->
			adjust (beginSleep t) >> adjust endSleep >> getObjects
		(Just _, Nothing) ->
			adjust (uncurry raiseError rlRstErr) >> getObjects
		(Nothing, _) ->
			adjust (uncurry raiseError rlRmnngErr) >> getObjects
	where
	api = ("https://api.github.com/users?since=" <>) . T.pack . show @Int
	rlRmnng = (read . BSC.unpack <$>) . lookup "X-RateLimit-Remaining"
	rlRst = (posixSeconds =<<) . lookup "X-RateLimit-Reset"
	rlRmnngErr = (NoRateLimitRemaining, "No X-RateLimit-Remaining header")
	rlRstErr = (NoRateLimitReset, "No X-RateLimit-Reset header")

---------------------------------------------------------------------------

type Clickable = (View, ReactF ())

view :: Clickable -> View
view = fst

click :: Clickable -> ReactF ()
click = snd

type WithTextExtents = (FontName, FontSize, T.Text, XGlyphInfo)

withTextExtents :: FontName -> FontSize -> T.Text -> ReactF WithTextExtents
withTextExtents fn fs t = (fn, fs, t,) <$> adjust (calcTextExtents fn fs t)

data Rect = Rect { upLeft :: Position, botRight :: Position } deriving Show

clickableText :: Position -> WithTextExtents -> Clickable
clickableText (x, y) (fn, fs, t, xg) = (
	[Text blue fn fs (x, y) t],
	clickOnRect $ Rect (x - gx, y - gy) (x - gx + gw, y - gy + gh) )
	where [gx, gy, gw, gh] = fromIntegral . ($ xg) <$> [
		xglyphinfo_x, xglyphinfo_y,
		xglyphinfo_width, xglyphinfo_height ]

translate :: Position -> WithTextExtents -> (Rational, Rational) -> Position
translate (x, y) (_, fs, _, _) (dx, dy) =
	(x + round (fs' * dx), y + round (fs' * dy)) where fs' = toRational fs

nextToText :: Position -> WithTextExtents -> Position
nextToText (x, y) (_, _, _, xg) =
	(x + fromXg xglyphinfo_xOff, y + fromXg xglyphinfo_yOff)
	where fromXg = fromIntegral . ($ xg)

clickOnRect :: Rect -> ReactF ()
clickOnRect (Rect (l, t) (r, b)) =
	() <$ find isInside (mousePosition `indexBy` repeat leftClick)
	where
	mousePosition :: SigF Position ()
	mousePosition = repeat $ adjust move
	isInside (x, y) = l <= x && x <= r && t <= y && y <= b

posixSeconds :: BS.ByteString -> Maybe UTCTime
posixSeconds =
	(posixSecondsToUTCTime . fromInteger <$>) . readMaybe . BSC.unpack
