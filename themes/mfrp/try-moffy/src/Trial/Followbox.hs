{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox (followbox) where

import Prelude hiding (break, until)

import Control.Monad (forever, (<=<))
import Control.Moffy
import Data.Type.Flip ((<$%>), (<*%>), ftraverse)
import Data.Or (Or(..))
import Data.Time (UTCTime, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Text.Read (readMaybe)
import Codec.Picture (decodeImage, convertRGBA8)
import Codec.Picture.Extra (scaleBilinear)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Moffy.EventHandle.Lock (LockId, newLockId, withLock)
import Moffy.EventHandle.Random (getRandomR)
import Moffy.Event.Mouse (deleteEvent)
import Trial.Followbox.Event (
	SigF, ReactF, clearJsons, storeJsons, loadJsons, httpGet, getTimeZone,
	browse, beginSleep, checkBeginSleep, endSleep,
	Error(..), raiseError, checkTerminate )
import Trial.Followbox.Clickable (
	Clickable, view, click, clickable, clickableText,
	WithTextExtents, withTextExtents, nextToText, translate )
import Trial.Followbox.View (View, View1(..), white)
import Trial.Followbox.TypeSynonym (
	Position, Avatar, FontName, FontSize, ErrorMessage )

---------------------------------------------------------------------------

-- * PARAMETER LIST
-- * SIG AND REACT
--	+ FOLLOWBOX
--	+ USERS
--	+ GET USER
-- * HELPER FUNCTION

---------------------------------------------------------------------------
-- PARAMETER LIST
---------------------------------------------------------------------------

numOfUsers :: Integer
numOfUsers = 3

userPageMax :: Int
userPageMax = 2 ^ (27 :: Int)

defaultFont :: FontName
defaultFont = "sans"

middleSize, largeSize :: FontSize
middleSize = 30; largeSize = 36

avatarSizeX, avatarSizeY :: Int
(avatarSizeX, avatarSizeY) = (80, 80)

titlePos, nextPos, refreshPos, resetTimePos :: Position
titlePos = (50, 80); nextPos = (500, 80)
refreshPos = (600, 80); resetTimePos = (100, 500)

avatarPos, namePos :: Integer -> Position
avatarPos n = (100, 120 * (n + 1))
namePos n = (210, 120 * (n + 1) + 60)

crossSize :: Integer
crossSize = round largeSize `div` 2

crossPos :: Position -> WithTextExtents -> Position
crossPos p wte = translate (nextToText p wte) wte (1 / 2, - 5 / 8)

crossMergin :: Integer
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
	let	frame = title : view nxt <> view rfs; clear = emit frame
	lck <- waitFor $ adjust newLockId
	(clear >>) . forever $ (frame <>)
		<$%> users lck n `until` click nxt `first` click rfs >>= \case
			Right (_, L _) -> pure ()
			Right (_, LR _ _) -> pure ()
			Right (_, R _) -> clear >> waitFor (adjust clearJsons)
			Left _ -> error "never occur"
	where
	title = twhite largeSize titlePos "Who to follow"
	link p t = clickableText p <$> withTextExtents defaultFont middleSize t

resetTime :: SigF s View ()
resetTime = forever $ emit [] >> do
	emit =<< waitFor do
		(t, tz) <- (,) <$> adjust checkBeginSleep <*> adjust getTimeZone
		pure [twhite middleSize resetTimePos . T.pack
			$ "Wait until " <> show (utcToLocalTime tz t)]
	waitFor $ adjust endSleep

-- USERS

users :: LockId -> Integer -> SigF s View ()
users lck n = concat <$%> user1 lck `ftraverse` [0 .. n - 1]

user1 :: LockId -> Integer -> SigF s View ()
user1 lck n = do
	waitFor . adjust . raiseError Trace $ "user1: n == " ++ show n
	(a, ln, u) <- waitFor $ getUser lck
	(nm, cr) <- waitFor $ nameCross n ln
	emit $ Image (avatarPos n) a : view nm <> view cr
	() <$ waitFor (forever $ click nm >> adjust (browse u)) `break` click cr
	user1 lck n

nameCross :: Integer -> T.Text -> ReactF s (Clickable s, Clickable s)
nameCross n t = (<$> withTextExtents defaultFont largeSize t) \wte ->
	(clickableText p wte, cross $ crossPos p wte)
	where p = namePos n

cross :: Position -> Clickable s
cross (l, t) = clickable [line lt rb, line lb rt] (l', t') (r', b')
	where
	line = Line white 4
	[lt, lb, rt, rb] = [(l, t), (l, b), (r, t), (r, b)]
	[r, b] = (+ crossSize) <$> [l, t]
	[l', t'] = subtract crossMergin <$> [l, t]
	[r', b'] = (+ crossMergin) <$> [r, b]

-- GET USER

{-# ANN getUser ("HLint: ignore Redundant <$>" :: String) #-}

getUser :: LockId -> ReactF s (Avatar, T.Text, T.Text)
getUser lck = makeUser <$> getObj1 lck >>= \case
	Left (e, em) -> adjust (raiseError e em) >> getUser lck
	Right (au, l, u) -> getAvatar au >>= \case
		Left (e, em) -> adjust (raiseError e em) >> getUser lck
		Right a -> pure (a, l, u)
	where
	makeUser o = (,,)
		<$> getField o "avatar_url" NoAvatarAddress "No Avatar Address"
		<*> getField o "login" NoLoginName "No Login Name"
		<*> getField o "html_url" NoHtmlUrl "No HTML URL"
	getField o k e em = case HM.lookup k o of
		Just (String li) -> Right li; _ -> Left (e, em)

getAvatar :: T.Text -> ReactF s (Either (Error, ErrorMessage) Avatar)
getAvatar url = (<$> adjust (httpGet url)) . (. bsToImage . snd) $ either
	(Left . (NoAvatar ,)) (Right . scaleBilinear avatarSizeX avatarSizeY)
	where bsToImage = (convertRGBA8 <$>) . decodeImage . LBS.toStrict

getObj1 :: LockId -> ReactF s Object
getObj1 lck = withLock lck $ adjust loadJsons >>= \case
	[] -> getObj1FromWeb; o : os -> o <$ adjust (storeJsons os)

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
		(Just _, Just t) -> do
			adjust (raiseError Trace "TRACE HERE 1")
			adjust (beginSleep t)
			adjust (raiseError Trace "TRACE HERE 2")
			adjust endSleep
			adjust (raiseError Trace "TRACE HERE 3")
			getObjs
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
twhite = Text white defaultFont

posixSeconds :: BS.ByteString -> Maybe UTCTime
posixSeconds =
	(posixSecondsToUTCTime . fromInteger <$>) . readMaybe . BSC.unpack
