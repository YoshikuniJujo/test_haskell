{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.FollowboxOrigin (
	-- * followbox
	followbox ) where

import Prelude hiding (break, until, repeat)

import Control.Arrow ((>>>), second, (***), (&&&))
import Control.Monad (void, forever, (<=<))
import Control.Moffy (React, adjust, adjustSig, emit, waitFor, first, break, until, indexBy, find, repeat)
import Control.Moffy.Event.Lock (LockId, newLockId, withLock)
import Control.Moffy.Samples.Event.Random (getRandomR)
import Control.Moffy.Samples.Event.Delete (deleteEvent)
import Control.Moffy.Samples.Viewable.Basic (Position)
import Control.Moffy.Samples.Followbox.Event (
	SigF, ReactF,
	clearJsons, storeJsons, loadJsons, httpGet, getTimeZone,
	browse, beginSleep, checkBeginSleep, endSleep,
	Error(..), raiseError, checkTerminate )
import Control.Moffy.Samples.Followbox.Clickable (
	Clickable, view, click, clickable, clickableText,
	WithTextExtents, withTextExtents, nextToText, translate,
	FontName, FontSize )
import Control.Moffy.Samples.Followbox.ViewType (
	View(..), View1, white, Png(..), VText(..), Line(..), Image(..) )
import Control.Moffy.Samples.Followbox.TypeSynonym (ErrorMessage, Uri)
import Data.Type.Set
import Data.Type.Flip ((<$%>), (<*%>), ftraverse)
import Data.OneOfThem
import Data.Or (Or(..))
import Data.HashMap.Strict qualified as HM
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Time (utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Data.Aeson.KeyMap (toHashMap)
import Text.Read (readMaybe)
import Codec.Picture qualified as P

import Data.Hashable
import Data.String

import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Area

---------------------------------------------------------------------------

-- * PARAMETER LIST
-- 	+ NUMBER OF USER TO DISPLAY
-- 	+ MAX NUMBER OF GITHUB USER
-- 	+ BACKGROUND
-- 	+ FONT
-- 	+ AVATAR, NAME AND CROSS
-- * SIG AND REACT
--	+ FOLLOWBOX
--	+ USERS
--	+ GET USER
--	+ GET OBJECT

---------------------------------------------------------------------------
-- PARAMETER LIST
---------------------------------------------------------------------------

-- NUMBER OF USER TO DISPLAY

numOfUsers :: Integer
numOfUsers = 3

-- MAX NUMBER OF GITHUB USER

userPageMax :: Int
userPageMax = 2 ^ (27 :: Int)

-- BACKGROUND

titlePos, nextPos, refreshPos, resetTimePos :: Position
titlePos = (50, 44); nextPos = (500, 44)
refreshPos = (600, 44); resetTimePos = (100, 470)

-- FONT

defaultFont :: FontName
defaultFont = "sans"

middleSize, largeSize :: FontSize
middleSize = 30; largeSize = 36

-- AVATAR, NAME AND CROSS

avatarSizeX, avatarSizeY :: Double
(avatarSizeX, avatarSizeY) = (80, 80)

avatarPos, namePos :: Double -> Position
avatarPos n = (100, 120 + 120 * n)
namePos n = (210, 150 + 120 * n)

crossSize :: Double
crossSize = largeSize / 2

crossPos :: Position -> WithTextExtents -> Position
crossPos p wte = translate (nextToText p wte) wte (1 / 2, 3 / 8)

crossMergin :: Double
crossMergin = 4

---------------------------------------------------------------------------
-- SIG AND REACT
---------------------------------------------------------------------------

-- FOLLOWBOX

followbox :: SigF s AreaView ()
followbox = () <$
	fieldWithResetTime numOfUsers `break` deleteEvent `break` checkTerminate

type AreaView = ([(Int, (Point, Point))], View)

fieldWithResetTime :: Integer -> SigF s AreaView ()
fieldWithResetTime n = (<>) <$%> field n <*%> (([] ,) <$%> resetTime)

field :: Integer -> SigF s AreaView ()
field n = do
	(nxt, rfs) <- waitFor
		$ (,) <$> link nextPos "Next" <*> link refreshPos "Refresh"
	let	frame = View [title] <> view nxt <> view rfs; clear = emit ([], frame)
	lck <- waitFor $ adjust newLockId
	(clear >>) . forever $ (second (frame <>))
		<$%> users lck n `until` click nxt `first` click rfs >>= \case
			Right (_, L _) -> pure ()
			Right (_, LR _ _) -> pure ()
			Right (_, R _) -> clear >> waitFor (adjust clearJsons)
			Left _ -> error "never occur"
	where
	title = twhite largeSize titlePos "Who to follow"
	link p t = clickableText p
		<$> adjust (withTextExtents defaultFont middleSize t)

resetTime :: SigF s View ()
resetTime = forever $ emit (View []) >> do
	emit =<< waitFor do
		(t, tz) <- (,) <$> adjust checkBeginSleep <*> adjust getTimeZone
		pure $ View [twhite middleSize resetTimePos . T.pack
			$ "Wait until " <> show (utcToLocalTime tz t)]
	waitFor $ adjust endSleep

twhite :: FontSize -> Position -> T.Text -> View1
twhite fs p = expand . Singleton . Text' white defaultFont fs p

-- USERS

users :: LockId -> Integer -> SigF s AreaView ()
users lck n =
	mconcat <$%> (forever . user1 lck) `ftraverse` [0 .. n - 1]

user1 :: LockId -> Integer -> SigF s AreaView ()
user1 lck (fromIntegral &&& fromIntegral -> (n, n')) = do
	emit ([], View [])
	(avt, nm, uri) <- waitFor $ getUser lck
	wte <- waitFor . adjust $ withTextExtents defaultFont largeSize nm
	let	ap = avatarPos n'; np = namePos n'
		lnk = clickableText np wte; cr = cross $ crossPos np wte
		ara = crossArea $ crossPos np wte
	emit $ ([(n, ara)], View [expand . Singleton $ Image' ap avt] <> view lnk <> view cr)
	void $ waitFor (listenForUserPage lnk uri) `break` clickCross n

clickCross :: Int -> ReactF s Int
clickCross i = do
	clickArea =<< adjust (getArea i)
	adjust $ getRandomR (0, 99)

clickArea :: (Point, Point) -> ReactF s ()
clickArea ((l, u), (r, d)) = (() <$)
	. adjust $ find isd $ fst <$%> repeat Mouse.move `indexBy` repeat leftClick
	where isd (x, y) = l <= x && x <= r && u <= y && y <= d

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b0 = do
	b <- Mouse.down
	bool (clickOn b0) (pure ()) (b == b0)

leftClick :: React s (Singleton Mouse.Down) ()
leftClick = clickOn Mouse.ButtonPrimary

listenForUserPage :: Clickable s -> Uri -> ReactF s ()
listenForUserPage nm u = forever $ adjust (click nm) >> adjust (browse u)

crossArea :: Position -> (Position, Position)
crossArea (l, t) = ((l', t'), (r', b'))
	where
	(lt, lb, rt, rb) = ((l, t), (l, b), (r, t), (r, b))
	(r, b) = (l + crossSize, t + crossSize)
	(l', t') = (l - crossMergin, t - crossMergin)
	(r', b') = (r + crossMergin, b + crossMergin)

cross :: Position -> Clickable s
cross (l, t) = clickable (View [lwhite lt rb, lwhite lb rt]) (l', t') (r', b')
	where
	(lt, lb, rt, rb) = ((l, t), (l, b), (r, t), (r, b))
	(r, b) = (l + crossSize, t + crossSize)
	(l', t') = (l - crossMergin, t - crossMergin)
	(r', b') = (r + crossMergin, b + crossMergin)
	lwhite p q = expand . Singleton $ Line' white 4 p q

-- GET USER

{-# ANN getUser ("HLint: ignore Redundant <$>" :: String) #-}

getUser :: LockId -> ReactF s (Png, T.Text, T.Text)
getUser lck = ex3 . toHashMap <$> getObj1 lck >>= err `either` \(au, nm, url) ->
	getAvatarPng au >>= either err (pure . (, nm, url))
	where
	err e = adjust (uncurry raiseError e) >> getUser lck

getUsers :: ReactF s [(Png, T.Text, T.Text)]
getUsers = (sequence . map (ex3 . toHashMap) <$> getObjs') >>= err `either` \aunmurls -> do
	let	(aus, nms, urls) = unzip3 aunmurls
	sequence <$> (mapM getAvatarPng aus) >>= either err (pure . uncurry3 zip3 . (, nms, urls))
	where
	err e = adjust (uncurry raiseError e) >> getUsers

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

ex3 :: (Hashable k, IsString k, IsString b) =>
	HM.HashMap k Value -> Either (Error, b) (T.Text, T.Text, T.Text)
ex3 o = (,,)
	<$> ex "avatar_url" (NoAvatarAddress, "No Avatar Address")
	<*> ex "login" (NoLoginName, "No Login Name")
	<*> ex "html_url" (NoHtmlUrl, "No HTML URL")
	where
	ex k e = case HM.lookup k o of Just (String v) -> Right v; _ -> Left e

getAvatarPng :: T.Text -> ReactF s (Either (Error, ErrorMessage) Png)
getAvatarPng url = (<$> adjust (httpGet url))
	$ snd >>> LBS.toStrict >>> convert >>> either
		(Left . (NoAvatar ,))
		(Right . Png avatarSizeX avatarSizeY)

convert :: BS.ByteString -> Either String BS.ByteString
convert img = LBS.toStrict . P.encodePng . P.convertRGB8 <$> P.decodeImage img

-- GET OBJECT

getObj1 :: LockId -> ReactF s Object
getObj1 lck = withLock lck $ adjust loadJsons >>= \case
	[] -> getObj1FromWeb
	o : os -> o <$ adjust (storeJsons os)

getObj1FromWeb :: ReactF s Object
getObj1FromWeb = getObjs >>= \case
	Right (o : os) -> o <$ adjust (storeJsons os)
	Right [] -> adjust (raiseError EmptyJson "Empty JSON") >> getObj1FromWeb
	Left em -> adjust (raiseError NotJson em) >> getObj1FromWeb

getObjs' :: ReactF s [Object]
getObjs' = getObjs >>= \case
	Right [] -> adjust (raiseError EmptyJson "Empty JSON") >> getObjs'
	Right os -> pure os
	Left em -> adjust (raiseError NotJson em) >> getObjs'

getObjs :: ReactF s (Either String [Object])
getObjs = do
	n <- adjust $ getRandomR (0, userPageMax)
	(hdr, bdy) <- adjust . httpGet $ api n
	case (rmng hdr, rst hdr) of
		(Just rmn, _) | rmn > (0 :: Int) -> pure $ eitherDecode bdy
		(Just _, Just t) ->
			adjust (beginSleep t) >> adjust endSleep >> getObjs
		(Just _, Nothing) -> adjust (uncurry raiseError rstE) >> getObjs
		(Nothing, _) -> adjust (uncurry raiseError rmngE) >> getObjs
	where
	api = ("https://api.github.com/users?since=" <>) . T.pack . show @Int
	rmng = (read . BSC.unpack <$>) . lookup "X-RateLimit-Remaining"
	rst = posixSeconds <=< lookup "X-RateLimit-Reset"
	rmngE = (NoRateLimitRemaining, "No X-RateLimit-Remaining header")
	rstE = (NoRateLimitReset, "No X-RateLimit-Reset header")
	posixSeconds = (posixSecondsToUTCTime . fromInteger <$>)
		. readMaybe . BSC.unpack
