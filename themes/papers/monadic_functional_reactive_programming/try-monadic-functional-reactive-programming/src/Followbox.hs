{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox (FollowboxEvent(..), usersView) where

import Prelude hiding (map, repeat, until)

import Codec.Picture.Extra
import System.Random

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Codec.Picture as JP

import Signal
import React
import AesonObject

import Check.Followbox.GetUsers

import XGlyphInfo

import FollowboxEvent
import FollowboxView

type SigF s n a r = Sig s (FollowboxEvent n) a r

left, top, textLeft, textTop, vertOff, avatarSize :: Integral n => n
left = 50
top = 120
textLeft = left + 120
textTop = top + 75
vertOff = 120
avatarSize = 90

getUsersJson :: (Show n, Ord n) => Int -> ReactF s n [Object]
getUsersJson s = (decodeUsers <$> httpGet (apiUsers s)) >>= \case
	Left em -> raiseError em >> getUsersJson s
	Right os -> pure os

getUser1 :: (Show n, Ord n) => ReactF s n (T.Text, JP.Image JP.PixelRGBA8, T.Text)
getUser1 = loadJsons >>= \case
	[] -> loadRandoms >>= \case
		r : rs -> storeRandoms rs >> getUsersJson r >>= \case
			(o : os) -> nameAndImageFromObject o >>= \case
				Left em -> raiseError em >> getUser1
				Right ni -> ni <$ storeJsons (take 4 os)
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
	Just (String au) -> (JP.decodeImage . LBS.toStrict <$> httpGet (T.unpack au)) >>= \case
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
	(\xo -> ((li, xo), av, hu)) <$> calcTextExtents "sans" 60 (T.unpack li)

getRect :: Integral n => n -> XGlyphInfo n -> Rect n
getRect n gi = Rect
	(textLeft - x, textTop - y + vertOff * n)
	(textLeft - x + w, textTop - y + h + vertOff * n)
	where
	w = xGlyphInfoWidth gi
	h = xGlyphInfoHeight gi
	x = xGlyphInfoX gi
	y = xGlyphInfoY gi

xRect :: Integral n => n -> XGlyphInfo n -> Rect n
xRect n gi = Rect
	(textLeft + xo + 60 - 20, textTop + 5 - 40 + vertOff * n)
	(textLeft + xo + 60 + 10, textTop + 5 - 10 + vertOff * n)
	where xo = xGlyphInfoXOff gi

nameAndImageToView :: Integral n => n -> ((T.Text, XGlyphInfo n), JP.Image JP.PixelRGBA8) -> View n
nameAndImageToView n ((t, XGlyphInfo {
	xGlyphInfoWidth = w,
	xGlyphInfoX = x,
	xGlyphInfoXOff = xo }), i) = [
		Text 0x3066D6 60 (textLeft, textTop + vertOff * n) t,
		Image (left, top + vertOff * n) i,
		Line 0x3066D6 4
			(textLeft - x, textTop + 6 + vertOff * n)
			(textLeft + w - x, textTop + 6 + vertOff * n),
		Line 0xFFFFFF 6
			(textLeft + xo + 60 - 20, textTop + 5 - 40 + vertOff * n)
			(textLeft + xo + 60 + 10, textTop + 5 - 10 + vertOff * n),
		Line 0xFFFFFF 6
			(textLeft + xo + 60 - 20, textTop + 5 - 10 + vertOff * n)
			(textLeft + xo + 60 + 10, textTop + 5 - 40 + vertOff * n)
		]

title :: Num n => View1 n
title = Text 0xFFFFFF 60 (10, 80) "Who to follow"

refresh :: (Show n, Ord n, Integral n) => ReactF s n (View n, Rect n)
refresh = do
	gi <- calcTextExtents "sans" 60 "Refresh"
	let	w = xGlyphInfoWidth gi
		h = xGlyphInfoHeight gi
		x = xGlyphInfoX gi
		y = xGlyphInfoY gi
	pure (	[	Text 0x3066D6 60 (600, 80) "Refresh",
			Line 0x3066D6 4
				(600 - x, 80 + 6)
				(600 + w - x, 80 + 6) ],
		Rect	(600 - x, 80 -  y)
			(600 - x + w, 80 - y + h) )

userViewReact :: (Show n, Ord n, Integral n) => n -> ReactF s n (View n, XGlyphInfo n, T.Text)
userViewReact n = (<$> nameAndImageReact)
	\(a@(_, gi), b, c) -> (nameAndImageToView n (a, b), gi, c)

usersView :: (Show n, Ord n, Integral n) => SigF s n (View n) ()
usersView = do
	waitFor (storeRandoms (randomRs (0, 499) (mkStdGen 8)))
	(r, rct) <- waitFor refresh
	tu r rct
	where
	tu r rct = do
		r0 <- waitFor (userViewReact 0)
		r1 <- waitFor (userViewReact 1)
		r2 <- waitFor (userViewReact 2)
		() <$ ((title :) . (r ++)) `map` (threeFields r0 r1 r2 `until` leftClickOn rct)
		emit $ title : r
		tu r rct

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
