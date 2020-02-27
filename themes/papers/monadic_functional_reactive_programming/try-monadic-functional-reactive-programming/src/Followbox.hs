{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox where

import Prelude hiding (map, repeat)

import Foreign.C.Types
import Codec.Picture.Extra
import System.Random

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Codec.Picture as JP

import Signal
import React
import Event
import AesonObject
import View

import Check.Followbox.GetUsers

import qualified Field as F

data FollowboxEvent
	= Http Uri (Event LBS.ByteString)
	| StoreRandoms (Action [Int]) | LoadRandoms (Event [Int])
	| StoreJsons (Action [Object]) | LoadJsons (Event [Object])
	| Prod
	| LeftClick
	| RightClick
	| Move (Event (CInt, CInt))
	| Browse (Action Uri)
	| CalcTextExtents (Bidirectional (FontName, FontSize, String) XGlyphInfo)
	deriving (Show, Eq, Ord)

data XGlyphInfo = XGlyphInfo {
	xGlyphInfoWidth :: Int,
	xGlyphInfoHeight :: Int,
	xGlyphInfoX :: Int,
	xGlyphInfoY :: Int,
	xGlyphInfoXOff :: Int,
	xGlyphInfoYOff :: Int
	} deriving (Show, Eq, Ord)

type FontName = String
-- type FontSize = Double

type ReactF s r = React s FollowboxEvent r
type SigF s a r = Sig s FollowboxEvent a r

type Uri = String

left, top, textLeft, textTop :: F.Position
left = 50
top = 100
textLeft = left + 120
textTop = top + 75

httpGet :: Uri -> ReactF s LBS.ByteString
httpGet u = pick <$> exper (S.singleton $ Http u Request)
	where pick evs = case S.elems $ S.filter (== Http u Request) evs of
		[Http _ (Occurred bs)] -> bs
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

storeRandoms :: [Int] -> ReactF s ()
storeRandoms rs = pick <$> exper (S.singleton $ StoreRandoms (Cause rs))
	where pick evs = case S.elems $ S.filter (== StoreRandoms (Cause rs)) evs of
		[StoreRandoms Response] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

loadRandoms :: ReactF s [Int]
loadRandoms = pick <$> exper (S.singleton $ LoadRandoms Request)
	where pick evs = case S.elems $ S.filter (== LoadRandoms Request) evs of
		[LoadRandoms (Occurred rs)] -> rs
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

storeJsons :: [Object] -> ReactF s ()
storeJsons os = pick <$> exper (S.singleton $ StoreJsons (Cause os))
	where pick evs = case S.elems $ S.filter (== StoreJsons (Cause os)) evs of
		[StoreJsons Response] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

loadJsons :: ReactF s [Object]
loadJsons = pick <$> exper (S.singleton $ LoadJsons Request)
	where pick evs = case S.elems $ S.filter (== LoadJsons Request) evs of
		[LoadJsons (Occurred os)] -> os
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

prod :: ReactF s ()
prod = pick <$> exper (S.singleton Prod)
	where pick evs = case S.elems $ S.filter (== Prod) evs of
		[Prod] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

rightClick :: ReactF s ()
rightClick = pick <$> exper (S.singleton RightClick)
	where pick evs = case S.elems $ S.filter (== RightClick) evs of
		[RightClick] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

browse :: Uri -> ReactF s ()
browse u = pick <$> exper (S.singleton . Browse $ Cause u)
	where pick evs = case S.elems $ S.filter (== Browse (Cause u)) evs of
		[Browse _] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

leftClick :: ReactF s ()
leftClick = pick <$> exper (S.singleton LeftClick)
	where pick evs = case S.elems $ S.filter (== LeftClick) evs of
		[LeftClick] -> ()
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

move :: ReactF s (CInt, CInt)
move = pick <$> exper (S.singleton $ Move Request)
	where pick evs = case S.elems $ S.filter (== Move Request) evs of
		[Move (Occurred p)] -> p
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

calcTextExtents :: FontName -> FontSize -> String -> ReactF s XGlyphInfo
calcTextExtents fn fs str = pick <$> exper (S.singleton $ CalcTextExtents (Action (fn, fs, str)))
	where pick  evs = case S.elems $ S.filter (== CalcTextExtents Communication) evs of
		[CalcTextExtents (Event xo)] -> xo
		es -> error $ "never occur: " ++ show es ++ " : " ++ show evs

getUsersJson :: Int -> ReactF s (Either String [Object])
getUsersJson s = decodeUsers <$> httpGet (apiUsers s)

getUser1 :: ReactF s (Maybe Object)
getUser1 = do
	oa <- loadJsons
	case oa of
		[] -> loadRandoms >>= \case
			r : rs -> do
				storeRandoms rs
				getUsersJson r >>= \case
					Right (o : os) ->
						Just o <$ storeJsons (take 4 os)
					Right [] -> error "no GitHub users"
					Left s -> error s
			[] -> error "no random numbers"
		o : os -> Just o <$ storeJsons os

getUser1' :: ReactF s (Either String (T.Text, JP.Image JP.PixelRGBA8, T.Text))
getUser1' = loadJsons >>= \case
	[] -> loadRandoms >>= \case
		r : rs -> do
			storeRandoms rs
			getUsersJson r >>= \case
				Right (o : os) -> nameAndImageFromObject o <* storeJsons (take 4 os)
				Right [] -> pure $ Left "no GitHub users"
				Left s -> pure $ Left s
		[] -> error "no random numbers"
	o : os -> nameAndImageFromObject o <* storeJsons os

nameAndImageFromObject :: Object -> ReactF s (Either String (T.Text, JP.Image JP.PixelRGBA8, T.Text))
nameAndImageFromObject o = do
	img <- avatorFromObject o
	pure $ (,,)
		<$> maybe (Left "no login name") Right (loginNameFromObject o)
		<*> img
		<*> maybe (Left "no html url") Right (htmlUrlFromObject o)

loginNameFromObject, htmlUrlFromObject :: Object -> Maybe T.Text
loginNameFromObject o = case HM.lookup "login" o of
	Just (String li) -> Just li
	_ -> Nothing

htmlUrlFromObject o = case HM.lookup "html_url" o of
	Just (String li) -> Just li
	_ -> Nothing

avatorFromObject :: Object -> ReactF s (Either String (JP.Image JP.PixelRGBA8))
avatorFromObject o = (scaleBilinear 100 100 <$>) <$> case HM.lookup "avatar_url" o of
	Just (String au) -> ((JP.convertRGBA8 <$>) . JP.decodeImage . LBS.toStrict) <$> httpGet (T.unpack au)
	_ -> pure $ Left "no avatar_url"

apiUsers :: Int -> Uri
apiUsers s = "https://api.github.com/users?since=" ++ show s

tryUsers :: SigF s (Maybe Object) ()
tryUsers = waitFor (storeRandoms (randomRs (0, 499) (mkStdGen 8))) >> tu
	where
	tu = do	u <- waitFor getUser1
		emit u
		waitFor $ loop u
		tu
	loop u = do
		(a, b) <- prod `first` rightClick
		case (done a, done b) of
			(Just (), _) -> pure ()
			_ -> do	case HM.lookup "html_url" =<< u of
					Just (String hu) -> browse $ T.unpack hu
					_ -> pure ()
				loop u

nameAndImage :: CInt -> SigF s (Either String ((T.Text, XGlyphInfo), JP.Image JP.PixelRGBA8)) ()
nameAndImage n = waitFor (storeRandoms (randomRs (0, 499) (mkStdGen 8))) >> tu
	where
	tu = waitFor getUser1' >>= \case
		Right (li, av, hu) -> do
			xo <- waitFor $ calcTextExtents "sans" 60 $ T.unpack li
			emit $ Right ((li, xo), av)
			waitFor $ loop (T.unpack hu) xo
			tu
		Left em -> emit $ Left em
	loop hu xo = do
		(a, b) <- leftClickOn (xRect n xo) `first` leftClickOn (getRect n xo)
		case (done a, done b) of
			(Just _, _) -> pure ()
			_ -> browse hu >> loop hu xo

getRect :: CInt -> XGlyphInfo -> Rect
getRect n gi = Rect
	(fromIntegral textLeft - x, fromIntegral textTop - y + 80 * n)
	(fromIntegral textLeft - x + w, fromIntegral textTop - y + h + 80 * n)
	where
	w = fromIntegral $ xGlyphInfoWidth gi
	h = fromIntegral $ xGlyphInfoHeight gi
	x = fromIntegral $ xGlyphInfoX gi
	y = fromIntegral $ xGlyphInfoY gi

xRect :: CInt -> XGlyphInfo -> Rect
xRect n gi = Rect
	(fromIntegral textLeft + xo + 60 - 20, fromIntegral textTop + 5 - 40 + 80 * n)
	(fromIntegral textLeft + xo + 60 + 10, fromIntegral textTop + 5 - 10 + 80 * n)
	where
	xo = fromIntegral $ xGlyphInfoXOff gi

nameAndImageToView :: CInt -> ((T.Text, XGlyphInfo), JP.Image JP.PixelRGBA8) -> View
nameAndImageToView n_ ((t, XGlyphInfo {
	xGlyphInfoWidth = w,
	xGlyphInfoHeight = h,
	xGlyphInfoX = x,
	xGlyphInfoY = y,
	xGlyphInfoXOff = xo,
	xGlyphInfoYOff = yo }), i) = [
		Text 0x3066D6 60 (textLeft, textTop + 80 * n) t,
		Image (left, top + 80 * n) i,
		Line 0x3066D6 4
			(textLeft - fromIntegral x, textTop + 6 + 80 * n)
			(textLeft + fromIntegral (w - x), textTop + 6 + 80 * n),
		Line 0xFFFFFF 6
			(textLeft + fromIntegral xo + 60 - 20, textTop + 5 - 40 + 80 * n)
			(textLeft + fromIntegral xo + 60 + 10, textTop + 5 - 10 + 80 * n),
		Line 0xFFFFFF 6
			(textLeft + fromIntegral xo + 60 - 20, textTop + 5 - 10 + 80 * n)
			(textLeft + fromIntegral xo + 60 + 10, textTop + 5 - 40 + 80 * n)
		]
	where n = fromIntegral n_

userView :: CInt -> SigF s (Either String View) ()
userView n = (nameAndImageToView n <$>) `map` nameAndImage n

usersView :: SigF s (Either String View) ()
-- usersView = () <$ always (\a b c -> (\x y z -> x ++ y ++ z) <$> a <*> b <*> c) <^> userView 0 <^> userView 1 <^> userView 2
-- usersView = () <$ always (\a b c -> a ++ b ++ c) <^> userView 0 <^> userView 1 <^> userView 2
usersView = userView 0

mousePos :: SigF s (CInt, CInt) ()
mousePos = repeat move

rightClickOn, leftClickOn :: Rect -> ReactF s (Either (CInt, CInt) ())
rightClickOn r = posInside r (mousePos `indexBy` repeat rightClick)
leftClickOn r = posInside r (mousePos `indexBy` repeat leftClick)

data Rect = Rect {
	leftUp :: (CInt, CInt),
	rightDown :: (CInt, CInt) } deriving Show

posInside :: Rect -> SigF s (CInt, CInt) y -> ReactF s (Either (CInt, CInt) y)
posInside r = find (`inside` r)

inside :: (CInt, CInt) -> Rect -> Bool
(x, y) `inside` Rect (l, u) (r, d) = l <= x && x <= r && u <= y && y <= d
