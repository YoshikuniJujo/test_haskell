{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox (followbox) where

import Prelude hiding (until, repeat)

import Control.Monad (void, forever, replicateM)
import Data.Type.Flip ((<$%>), (<*%>), ftraverse)
import Data.Type.Set (Set(Nil), (:-))
import Data.Or (Or(..))
import Data.Time (utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Graphics.X11.Xrender (XGlyphInfo(..))

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Codec.Picture as JP
import qualified Codec.Picture.Extra as JP

import MonadicFrp (
	React, adjust, first, emit, waitFor, find, repeat, until, indexBy )
import Trials.Followbox.Event (
	SigF, ReactF, move, leftClick, storeJsons, loadJsons,
	httpGet, calcTextExtents, GetTimeZone, getTimeZone, browse,
	BeginSleep, beginSleep, checkBeginSleep, endSleep, checkQuit,
	Error(..), ErrorResult(..), raiseError, catchError )
import Trials.Followbox.View (View, View1(..), white, blue)
import Trials.Followbox.Random (getRandomR)
import Trials.Followbox.TypeSynonym (Position, Avatar, ErrorMessage)

---------------------------------------------------------------------------

followbox :: SigF View ()
followbox = () <$ usersAndResetTime 3 `until` checkQuit `until` checkTerminate
	where
	checkTerminate :: ReactF ()
	checkTerminate = adjust catchError >>=
		\case Continue -> checkTerminate; Terminate -> pure ()

usersAndResetTime :: Integer -> SigF View ()
usersAndResetTime n = (<>) <$%> users n <*%> resetTime

resetTime :: SigF View ()
resetTime = forever do
	emit [] >> (emit =<< waitFor (adjust makeResetTime))
	waitFor $ adjust endSleep
	where
	makeResetTime :: React (BeginSleep :- GetTimeZone :- 'Nil) View
	makeResetTime = do
		(t, tz) <- (,) <$> adjust checkBeginSleep  <*> adjust getTimeZone
		pure [Text white "sans" 30 (100, 500)
			$ "Wait until " <> T.pack (show $ utcToLocalTime tz t)]

users :: Integer -> SigF View ()
users n = do
	(vn, rn) <- waitFor next
	(vr, rr) <- waitFor refresh
	emit $ title : vn <> vr
	vmlns (vn, rn) (vr, rr)
	where
	vmlns nxt@(vn, rn) rfs@(vr, rr) = do
		lns <- fromIntegral n `replicateM` waitFor getUser
		r <- ((title :) . (vn <>) . (vr <>) . concat <$%> ftraverse (uncurry viewFields) ([0 .. n - 1] `zip` lns))
			`until` (clickOnRect rn `first` clickOnRect rr)
		case r of
			Left (_, L _) -> pure ()
			Left (_, LR _ _) -> pure ()
			Left (_, R _) -> do
				emit $ title : vn <> vr
				waitFor . adjust $ storeJsons []
			Right _ -> error "never occur"
		vmlns nxt rfs
	title = Text white "sans" 36 (50, 80) "Who to follow"
	next = linkText 30 (500, 80) "Next"
	refresh = linkText 30 (600, 80) "Refresh"

viewFields :: Integer -> (Avatar, T.Text, T.Text) -> SigF View ()
viewFields n (a, ln, u) = do
	(v, r, r') <- waitFor $ viewField1 n ln
	emit $ Image (100, 120 + 120 * n) a : v
	void $ waitFor (forever $ clickOnRect r' >> adjust (browse $ T.unpack u)) `until` clickOnRect r
	viewFields n =<< waitFor getUser

viewField1 :: Integer -> T.Text -> ReactF (View, Rect, Rect)
viewField1 n t = do
	XGlyphInfo {
		xglyphinfo_width = w_,
		xglyphinfo_height = h_,
		xglyphinfo_x = x_,
		xglyphinfo_y = y_ } <- adjust $ calcTextExtents "sans" 36 t
	let	[w, h, x, y] = fromIntegral <$> [w_, h_, x_, y_]
	pure $ createLoginName blue 36 (210, 180 + n * 120) (210 - x + w, 180 + n * 120 - y)
		(Rect (210 - x, 180 + n * 120 - y) (210 - x + w, 180 + n * 120 - y + h))
	where
	createLoginName clr fs p (x', y') r' = (
		Text clr "sans" fs p t : createX 4 (round fs `div` 2) (x' + round fs `div` 2, y' + round fs * 3 `div` 8),
		Rect	(x' + round fs `div` 2, y' + round fs * 3 `div` 8)
			(x' + round fs, y' + round fs * 7 `div` 8),
		r' )
		where
		createX lw sz (x, y) = [
			Line white lw (x, y) (x + sz, y + sz),
			Line white lw (x + sz, y) (x, y + sz) ]

data Rect = Rect { upperLeft :: Position, bottomRight :: Position }
	deriving Show

clickOnRect :: Rect -> ReactF ()
clickOnRect (Rect (l, t) (r, b)) = () <$ find isInside (mousePosition `indexBy` repeat leftClick)
	where
	mousePosition :: SigF Position ()
	mousePosition = repeat $ adjust move
	isInside (x, y) = l <= x && x <= r && t <= y && y <= b

linkText :: Double -> Position -> T.Text -> ReactF (View, Rect)
linkText fs p@(x0, y0) t = do
	XGlyphInfo {
		xglyphinfo_width = w_,
		xglyphinfo_height = h_,
		xglyphinfo_x = x_,
		xglyphinfo_y = y_ } <- adjust $ calcTextExtents "sans" fs t
	let	[w, h, x, y] = fromIntegral <$> [w_, h_, x_, y_]
	pure (	[	Text blue "sans" fs p t,
			Line blue 4
				(x0 - x, y0 + 6)
				(x0 + w - x, y0 + 6) ],
		Rect	(x0 - x, y0 - y)
			(x0 - x + w, y0 - y + h) )

getUser :: ReactF (Avatar, T.Text, T.Text)
getUser = makeUser <$> getObject1 >>= \case
	Left (e, em) -> adjust (raiseError e em) >> getUser
	Right (au, l, u) -> do
		getAvatar au >>= \case
			Left (e, em) -> adjust (raiseError e em) >> getUser
			Right a -> pure (a, l, u)
	where
	makeUser o = do
		a <- maybe (Left (NoAvatarAddress, "No Avatar Address"))
			Right $ o `lookupObject` "avatar_url"
		l <- maybe (Left (NoLoginName, "No Login Name"))
			Right $ o `lookupObject` "login"
		u <- maybe (Left (NoHtmlUrl, "No HTML URL"))
			Right $ o `lookupObject` "html_url"
		pure (a, l, u)
	lookupObject o k = case HM.lookup k o of
		Just (String li) -> Just li; _ -> Nothing

getAvatar :: T.Text -> ReactF (Either (Error, ErrorMessage) Avatar)
getAvatar url = do
	ea <- (scale 80 80 <$>) . bsToImage . snd <$> adjust (httpGet $ T.unpack url)
	case ea of
		Left em -> pure $ Left (NoAvatar, em)
		Right v -> pure $ Right v
	where
	scale :: Integer -> Integer -> JP.Image JP.PixelRGBA8 -> JP.Image JP.PixelRGBA8
	scale w_ h_ = JP.scaleBilinear w h where [w, h] = fromIntegral <$> [w_, h_]
	bsToImage :: LBS.ByteString -> Either String (JP.Image JP.PixelRGBA8)
	bsToImage lbs = JP.convertRGBA8 <$> JP.decodeImage (LBS.toStrict lbs)

getObject1 :: ReactF Object
getObject1 = adjust loadJsons >>= \case
	[] -> adjust getObjects >>= \case
		Right (o : os) -> o <$ adjust (storeJsons $ take 8 os)
		Right [] -> adjust (raiseError EmptyJson "Empty JSON") >> getObject1
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
	apiUsers = ("https://api.github.com/users?since=" <>) . show @Int
