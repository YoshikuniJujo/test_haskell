{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox (FollowboxEvent(..), XGlyphInfo(..), usersView, usersView') where

import Prelude hiding (map, repeat, until)

import Data.String
import Data.Time hiding (getCurrentTime)
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import Codec.Picture.Extra
import System.Random hiding (next)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Codec.Picture as JP

import Followbox.Event
import Followbox.View
import Signal
import React

type SigF s n a r = Sig s (FollowboxEvent n) a r

left, top, textLeft, textTop, vertOff, avatarSize :: Integral n => n
left = 50
top = 120
textLeft = left + 120
textTop = top + 75
vertOff = 120
avatarSize = 90

fontSize :: Num n => n
fontSize = 30

getUsersJson :: (Show n, Ord n) => Int -> ReactF s n [Object]
getUsersJson s = do
	waitMessage True
	now <- getCurrentTime
	rst <- loadRateLimitReset
	case rst of
		Just r | r > now -> waitMessage True >> sleep r >> getUsersJson s
		_ -> do	(hds, bd) <- httpGet $ apiUsers s
			let	rrmn = read . BSC.unpack <$> lookup (fromString "X-RateLimit-Remaining") hds :: Maybe Int
				rrst = posixSecondsToUTCTime . fromInteger . read . BSC.unpack <$> lookup (fromString "X-RateLimit-Reset") hds
			case (rrmn, rrst, decodeJson bd) of
				(Just rm, Just _, _) | rm < 1 -> storeRateLimitReset rrst >> waitMessage True >> getUsersJson s
				(_, _, Right os) -> storeRateLimitReset Nothing >> waitMessage True >> pure os
				(_, _, Left em) -> raiseError em >> getUsersJson s

getUser1 :: (Show n, Ord n) => ReactF s n (T.Text, JP.Image JP.PixelRGBA8, T.Text)
getUser1 = loadJsons >>= \case
	[] -> loadRandoms >>= \case
		r : rs -> storeRandoms rs >> getUsersJson r >>= \case
			o : os -> nameAndImageFromObject o >>= \case
				Left em -> raiseError em >> getUser1
				Right ni -> ni <$ storeJsons (take 8 os)
			[] -> raiseError "no GitHub users" >> getUser1
		[] -> raiseError "no random numbers" >> getUser1
	o : os -> nameAndImageFromObject o >>= \case
		Left em -> raiseError em >> storeJsons [] >> getUser1
		Right ni -> ni <$ storeJsons os

nameAndImageFromObject :: (Show n, Ord n) => Object ->
	ReactF s n (Either String (T.Text, JP.Image JP.PixelRGBA8, T.Text))
nameAndImageFromObject o = (<$> avatorFromObject o) \img ->
	(,,) <$> loginNameFromObject o <*> img <*> htmlUrlFromObject o

loginNameFromObject, htmlUrlFromObject :: Object -> Either String T.Text
loginNameFromObject o = case HM.lookup "login" o of
	Just (String li) -> Right li; _ -> Left "no login name"

htmlUrlFromObject o = case HM.lookup "html_url" o of
	Just (String li) -> Right li; _ -> Left "no html url"

avatorFromObject :: (Show n, Ord n) => Object -> ReactF s n (Either String (JP.Image JP.PixelRGBA8))
avatorFromObject o = (scaleBilinear avatarSize avatarSize <$>) <$> case HM.lookup "avatar_url" o of
	Just (String au) -> (JP.decodeImage . LBS.toStrict . snd <$> httpGet (T.unpack au)) >>= \case
		Left em -> pure $ Left em
		Right img -> pure . Right $ JP.convertRGBA8 img
	_ -> pure $ Left "no avatar_url"

apiUsers :: Int -> Uri
apiUsers s = "https://api.github.com/users?since=" ++ show s

userLoop :: (Show n, Integral n) => n -> Uri -> XGlyphInfo n -> ReactF s n ()
userLoop n hu xo = do
	(a, b) <- leftClickOn (xRect n xo) `first` leftClickOn (getRect n xo)
	case (done a, done b) of
		(Just _, _) -> pure ()
		_ -> browse hu >> userLoop n hu xo

nameAndImageReact ::
	(Show n, Ord n) => ReactF s n ((T.Text, XGlyphInfo n), JP.Image JP.PixelRGBA8, T.Text)
nameAndImageReact = getUser1 >>= \(li, av, hu) ->
	(\xo -> ((li, xo), av, hu)) <$> calcTextExtents "sans" fontSize (T.unpack li)

getRect :: Integral n => n -> XGlyphInfo n -> Rect n
getRect n gi = Rect
	(textLeft - x, textTop - y - dy' + dy + vertOff * n)
	(textLeft - x + w, textTop - y - dy' + dy + h + vertOff * n)
	where
	w = xGlyphInfoWidth gi
	h = xGlyphInfoHeight gi
	x = xGlyphInfoX gi
	y = xGlyphInfoY gi
	dy = fontSize `div` 2
	dy' = avatarSize `div` 3

xRect :: Integral n => n -> XGlyphInfo n -> Rect n
xRect n gi = Rect
	(textLeft + xo + fontSize - 5 - xsz, textTop - fontSize - xsz + vertOff * n)
	(textLeft + xo + fontSize - 5 + xsz, textTop - fontSize + xsz + vertOff * n)
	where
	xo = xGlyphInfoXOff gi
	xsz = fontSize `div` 4

nameAndImageToView :: Integral n => n -> ((T.Text, XGlyphInfo n), JP.Image JP.PixelRGBA8) -> View n
nameAndImageToView n ((t, XGlyphInfo {
	xGlyphInfoWidth = w,
	xGlyphInfoX = x,
	xGlyphInfoXOff = xo }), i) = [
		Text blue fontSize (textLeft, textTop - dy' + dy + vertOff * n) t,
		Image (left, top + vertOff * n) i,
		Line blue 4
			(textLeft - x, textTop + 6 - dy' + dy + vertOff * n)
			(textLeft + w - x, textTop + 6 - dy' + dy + vertOff * n),
		Line white xlw
			(textLeft + xo + fontSize - 5 - xsz, textTop - fontSize - xsz + vertOff * n)
			(textLeft + xo + fontSize - 5 + xsz, textTop - fontSize + xsz + vertOff * n),
		Line white xlw
			(textLeft + xo + fontSize - 5 - xsz, textTop - fontSize + xsz + vertOff * n)
			(textLeft + xo + fontSize - 5 + xsz, textTop - fontSize - xsz + vertOff * n)
		]
	where
	xsz = fontSize `div` 3
	xlw = fontSize `div` 10
	dy = fontSize `div` 2
	dy' = avatarSize `div` 3

title :: Num n => View1 n
title = Text white fontSize (10, 80) "Who to follow"

white, blue :: Color
white = Color { colorRed = 0xff, colorGreen = 0xff, colorBlue = 0xff }
blue = Color { colorRed = 0x30, colorGreen = 0x66, colorBlue = 0xd6 }

next :: (Show n, Ord n, Integral n) => ReactF s n (View n, Rect n)
next = do
	gi <- calcTextExtents "sans" 30 "Next"
	let	w = xGlyphInfoWidth gi
		h = xGlyphInfoHeight gi
		x = xGlyphInfoX gi
		y = xGlyphInfoY gi
	pure (	[	Text blue 30 (600, 80) "Next",
			Line blue 4
				(600 - x, 80 + 6)
				(600 + w - x, 80 + 6) ],
		Rect	(600 - x, 80 -  y)
			(600 - x + w, 80 - y + h) )

refresh :: (Show n, Ord n, Integral n) => ReactF s n (View n, Rect n)
refresh = do
	gi <- calcTextExtents "sans" 30 "Refresh"
	let	w = xGlyphInfoWidth gi
		h = xGlyphInfoHeight gi
		x = xGlyphInfoX gi
		y = xGlyphInfoY gi
	pure (	[	Text blue 30 (700, 80) "Refresh",
			Line blue 4
				(700 - x, 80 + 6)
				(700 + w - x, 80 + 6) ],
		Rect	(700 - x, 80 -  y)
			(700 - x + w, 80 - y + h) )

userViewReact :: (Show n, Ord n, Integral n) => n -> ReactF s n (View n, XGlyphInfo n, T.Text)
userViewReact n = (<$> nameAndImageReact)
	\(a@(_, gi), b, c) -> (nameAndImageToView n (a, b), gi, c)

first' :: (Ord e, Update a b) => React s e a -> React s e b -> React s e (Either a b)
l `first'` r = do
	(a, b) <- l `first` r
	case (done a, done b) of
		(Just x, _) -> pure $ Left x
		(_, Just y) -> pure $ Right y
		_ -> error "never occur"

until' :: (Ord e, Update (ISig s e a b) c) =>
	Sig s e a b -> React s e c -> Sig s e a (Maybe c)
l `until'` r = do
	(_, b) <- l `until` r
	pure $ done b

usersView :: (Show n, Ord n, Integral n) => SigF s n (View n) ()
usersView = do
	waitFor (storeRandoms (randomRs (0, 2 ^ (27 :: Int)) (mkStdGen 8)))
	(n, nxt) <- waitFor next
	(r, rct) <- waitFor refresh
	emit $ title : n ++ r
	tu n nxt r rct
	where
	tu n nxt r rct = do
		r0 <- waitFor (userViewReact 0)
		r1 <- waitFor (userViewReact 1)
		r2 <- waitFor (userViewReact 2)
		rslt <- ((title :) . (n ++) . (r ++)) `map` (threeFields r0 r1 r2 `until'` (leftClickOn nxt `Followbox.first'` leftClickOn rct))
		case rslt of
			Just (Left _) -> pure ()
			Just (Right _) -> do
				emit $ title : n ++ r
				waitFor $ storeJsons []
			_ -> error "never occur"
		tu n nxt r rct

waitMessageView :: (Show n, Ord n, Num n) => SigF s n (View n) ()
waitMessageView = do
	waitFor $ waitMessage False
--	emit []
--	now <- waitFor getCurrentTime
--	waitFor . sleep $ addUTCTime 1 now
	w <- waitFor loadRateLimitReset
	tz <- waitFor Followbox.Event.getCurrentTimeZone
	case w of
		Nothing -> emit []
		Just t -> emit [Text white 20 (100, 500) $ "Wait until " <>
			T.pack (show $ utcToZonedTime tz t)]
	waitMessageView

usersView' :: (Show n, Ord n, Integral n) => SigF s n (View n) ()
usersView' = () <$ always (++) <^> usersView <^> waitMessageView

threeFields :: (Show n, Ord n, Integral n) => (View n, XGlyphInfo n, T.Text) ->
	(View n, XGlyphInfo n, T.Text) ->
	(View n, XGlyphInfo n, T.Text) -> SigF s n (View n) ()
threeFields r0 r1 r2 = () <$ always (\a b c -> a ++ b ++ c)
	<^> applyUserViewN 0 r0 <^> applyUserViewN 1 r1 <^> applyUserViewN 2 r2

applyUserViewN :: (Show n, Ord n, Integral n) => n -> (View n, XGlyphInfo n, T.Text) -> SigF s n (View n) ()
applyUserViewN n (v, gi, hu) = userViewN n v gi hu

userViewN :: (Show n, Ord n, Integral n) => n -> View n -> XGlyphInfo n -> T.Text -> SigF s n (View n) ()
userViewN n v gi hu = emit v >> do
	waitFor $ userLoop n (T.unpack hu) gi
	(v', gi', hu') <- waitFor $ userViewReact n
	userViewN n v' gi' hu'

mousePos :: (Show n, Ord n) => SigF s n (n, n) ()
mousePos = repeat move

leftClickOn :: (Show n, Ord n) => Rect n -> ReactF s n (Either (n, n) ())
leftClickOn r = posInside r (mousePos `indexBy` repeat leftClick)

data Rect n = Rect { leftUp :: (n, n), rightDown :: (n, n) } deriving Show

posInside :: Ord n => Rect n -> SigF s n (n, n) y -> ReactF s n (Either (n, n) y)
posInside r = find (`inside` r)

inside :: Ord n => (n, n) -> Rect n -> Bool
(x, y) `inside` Rect (l, u) (r, d) = l <= x && x <= r && u <= y && y <= d
