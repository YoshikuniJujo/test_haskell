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

data FollowboxEvent
	= Http Uri (Event LBS.ByteString)
	| StoreRandoms (Action [Int]) | LoadRandoms (Event [Int])
	| StoreJsons (Action [Object]) | LoadJsons (Event [Object])
	| Prod
	| LeftClick
	| RightClick
	| Move (Event (CInt, CInt))
	| Browse (Action Uri)
	deriving (Show, Eq, Ord)

type ReactF s r = React s FollowboxEvent r
type SigF s a r = Sig s FollowboxEvent a r

type Uri = String

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

nameAndImage :: CInt -> SigF s (Either String (T.Text, JP.Image JP.PixelRGBA8)) ()
nameAndImage n = waitFor (storeRandoms (randomRs (0, 499) (mkStdGen 8))) >> tu
	where
	tu = waitFor getUser1' >>= \case
		Right (li, av, hu) -> do
			emit $ Right (li, av)
			waitFor . loop $ T.unpack hu
			tu
		Left em -> emit $ Left em
	loop hu = do
		(a, b) <- leftClick `first` rightClickOn (getRect n)
		case (done a, done b) of
			(Just (), _) -> pure ()
			_ -> browse hu >> loop hu

getRect :: CInt -> Rect
getRect n = Rect (0, 160 * n) (300, 160 * (n + 1))

nameAndImageToView :: (T.Text, JP.Image JP.PixelRGBA8) -> View
nameAndImageToView (t, i) = [Text 60 (170, 125) t, Image (50, 50) i]

userView :: SigF s (Either String View) ()
userView = (nameAndImageToView <$>) `map` nameAndImage 0

mousePos :: SigF s (CInt, CInt) ()
mousePos = repeat move

rightClickOn :: Rect -> ReactF s (Either (CInt, CInt) ())
rightClickOn r = posInside r (mousePos `indexBy` repeat rightClick)

data Rect = Rect {
	leftUp :: (CInt, CInt),
	rightDown :: (CInt, CInt) } deriving Show

posInside :: Rect -> SigF s (CInt, CInt) y -> ReactF s (Either (CInt, CInt) y)
posInside r = find (`inside` r)

inside :: (CInt, CInt) -> Rect -> Bool
(x, y) `inside` Rect (l, u) (r, d) = l <= x && x <= r && u <= y && y <= d
