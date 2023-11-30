{-# LANGUAGE BlockArguments, LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox (
	-- * followbox
	followbox ) where

import Prelude hiding (break, until)

import Control.Arrow ((>>>))
import Control.Monad (void, forever, (<=<))
import Control.Moffy (adjust, emit, waitFor, first, break, until)
import Control.Moffy.Event.Window
import Control.Moffy.Event.Lock (LockId, newLockId, withLock)
import Control.Moffy.Event.Random (getRandomR)
import Control.Moffy.Samples.Event.Delete (deleteEvent)
import Data.Type.Flip ((<$%>), (<*%>), ftraverse)
import Data.Or (Or(..))
import Data.Aeson (Object, Value(..), eitherDecode)
import Data.Time (UTCTime, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.Read (readMaybe)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Control.Moffy.Samples.Followbox.Event (
	SigF, ReactF, clearJsons, storeJsons, loadJsons, httpGet, getTimeZone,
	browse, beginSleep, checkBeginSleep, endSleep,
	Error(..), raiseError, checkTerminate )
import Control.Moffy.Samples.Followbox.Clickable (
	Clickable, view, click, clickable, clickableText,
	WithTextExtents, withTextExtents, nextToText, translate, FontName, FontSize )
import Trial.Followbox.ViewType (View(..), View1, white, Png(..), VText(..), Line(..), Image(..))
import Trial.Followbox.TypeSynonym (ErrorMessage)

import Control.Moffy.Viewable.Basic (Position)

import qualified Codec.Picture as P

import Data.OneOfThem

import Data.Aeson.KeyMap (toHashMap)

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
-- * HELPER FUNCTION

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

followbox :: SigF s View ()
followbox = () <$
	fieldWithResetTime numOfUsers `break` deleteEvent `break` checkTerminate

fieldWithResetTime :: Integer -> SigF s View ()
fieldWithResetTime n = (<>) <$%> field n <*%> resetTime

field :: Integer -> SigF s View ()
field n = do
	(nxt, rfs) <- waitFor
		$ (,) <$> link nextPos "Next" <*> link refreshPos "Refresh"
	let	frame = View [title] <> view nxt <> view rfs; clear = emit frame
	lck <- waitFor $ adjust newLockId
	(clear >>) . forever $ (frame <>)
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

-- USERS

users :: LockId -> Integer -> SigF s View ()
users lck n = mconcat <$%> (forever . user1 lck) `ftraverse` [0 .. n - 1]

user1 :: LockId -> Integer -> SigF s View ()
user1 lck n = do
	(a, ln, u) <- waitFor $ getUser lck
	wte <- waitFor . adjust $ withTextExtents defaultFont largeSize ln
	let	nm = clickableText np wte; cr = cross $ crossPos np wte
	emit $ View [expand . Singleton $ Image' (avatarPos $ fromIntegral n) a] <> view nm <> view cr
	void . (`break` click cr) . waitFor $ forever
		(adjust (click nm) >> adjust (browse u) :: ReactF s ())
	where np = namePos $ fromIntegral n

cross :: Position -> Clickable s
cross (l, t) = clickable (View [lwhite lt rb, lwhite lb rt]) (l', t') (r', b')
	where
	[lt, lb, rt, rb] = [(l, t), (l, b), (r, t), (r, b)]
	[r, b] = (+ crossSize) <$> [l, t]
	[l', t'] = subtract crossMergin <$> [l, t]
	[r', b'] = (+ crossMergin) <$> [r, b]

-- GET USER

{-# ANN getUser ("HLint: ignore Redundant <$>" :: String) #-}

getUser :: LockId -> ReactF s (Png, T.Text, T.Text)
getUser lck = ex3 . toHashMap <$> getObj1 lck >>= err `either` \(au, ln, u) ->
	getAvatarPng au >>= either err (pure . (, ln, u))
	where
	ex3 o = (,,)
		<$> ex o "avatar_url" (NoAvatarAddress, "No Avatar Address")
		<*> ex o "login" (NoLoginName, "No Login Name")
		<*> ex o "html_url" (NoHtmlUrl, "No HTML URL")
	ex o k e = case HM.lookup k o of Just (String v) -> Right v; _ -> Left e
	err e = adjust (uncurry raiseError e) >> getUser lck

getAvatarPng :: T.Text -> ReactF s (Either (Error, ErrorMessage) Png)
getAvatarPng url = (<$> adjust (httpGet url))
	$ snd >>> LBS.toStrict >>> convert >>> either
		(Left . (NoAvatar ,))
		(Right . Png avatarSizeX avatarSizeY)

convert :: BS.ByteString -> Either String BS.ByteString
convert img = LBS.toStrict . P.encodePng . P.convertRGB8 <$> P.decodeImage img

-- GET OBJECT

getObj1 :: LockId -> ReactF s Object
getObj1 lck = withLock lck $ adjust loadJsons
	>>= \case [] -> getObj1FromWeb; o : os -> o <$ adjust (storeJsons os)

getObj1FromWeb :: ReactF s Object
getObj1FromWeb = getObjs >>= \case
	Right (o : os) -> o <$ adjust (storeJsons os)
	Right [] -> adjust (raiseError EmptyJson "Empty JSON") >> getObj1FromWeb
	Left em -> adjust (raiseError NotJson em) >> getObj1FromWeb

getObjs :: ReactF s (Either String [Object])
getObjs = do
	n <- adjust $ getRandomR (0, userPageMax)
	(h, b) <- adjust . httpGet $ api n
	case (rmng h, rst h) of
		(Just rmn, _) | rmn > (0 :: Int) -> pure $ eitherDecode b
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

---------------------------------------------------------------------------
-- HELPER FUNCTION
---------------------------------------------------------------------------

twhite :: FontSize -> Position -> T.Text -> View1
twhite fs p = expand . Singleton . Text' white defaultFont fs p

lwhite :: Position -> Position -> View1
lwhite p q = expand . Singleton $ Line' white 4 p q

posixSeconds :: BS.ByteString -> Maybe UTCTime
posixSeconds =
	(posixSecondsToUTCTime . fromInteger <$>) . readMaybe . BSC.unpack
