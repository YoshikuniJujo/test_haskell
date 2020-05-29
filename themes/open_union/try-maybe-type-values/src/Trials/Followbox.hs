{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox (followbox) where

import Prelude hiding (until, repeat)

import Control.Monad (forever)
import Data.Type.Flip ((<$%>), (<*%>), ftraverse)
import Data.Or (Or(..))
import Data.Time (UTCTime, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Text.Read (readMaybe)
import Codec.Picture (convertRGBA8, decodeImage)
import Codec.Picture.Extra (scaleBilinear)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import MonadicFrp (adjust, first, emit, waitFor, until)
import MonadicFrp.EventHandle.Lock
import Trials.Followbox.Clickable (
	Clickable, view, click, clickable,
	WithTextExtents, clickableText, withTextExtents, nextToText, translate )
import Trials.Followbox.Event (
	SigF, ReactF, clearJsons, storeJsons, loadJsons,
	httpGet, getTimeZone, browse,
	beginSleep, checkBeginSleep, endSleep, deleteEvent,
	Error(..), raiseError, checkTerminate )
import Trials.Followbox.View (View, View1(..), white)
import Trials.Followbox.Random (getRandomR)
import Trials.Followbox.TypeSynonym (
	Position, Avatar, FontName, FontSize, ErrorMessage )

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
	fieldWithResetTime numOfUsers `until` deleteEvent `until` checkTerminate

fieldWithResetTime :: Integer -> SigF View ()
fieldWithResetTime n = (<>) <$%> field n <*%> resetTime

resetTime :: SigF View ()
resetTime = forever $ (emit [] >>) do
	emit =<< waitFor do
		(t, tz) <- (,) <$> adjust checkBeginSleep <*> adjust getTimeZone
		pure [twhite middleSize resetTimePosition . T.pack
			$ "Wait until " <> show (utcToLocalTime tz t)]
	waitFor $ adjust endSleep

data Locks = Locks { lockRandom :: LockId, lockObjects :: LockId } deriving Show

newLocks :: ReactF Locks
newLocks = Locks <$> adjust newLockId <*> adjust newLockId

field :: Integer -> SigF View ()
field n = do
	(nxt, rfs) <- waitFor $ (,)
		<$> link nextPosition "Next"
		<*> link refreshPosition "Refresh"
	let	frame = title : view nxt <> view rfs
		clear = emit frame
	ls <- waitFor newLocks
	clear >> forever do
		(frame <>) <$%>
			users' ls n `until` click nxt `first` click rfs >>= \case
				Left (_, L _) -> pure ()
				Left (_, LR _ _) -> pure ()
				Left (_, R _) ->
					clear >> waitFor (adjust clearJsons)
				Right _ -> error "never occur"
	where
	title = twhite largeSize titlePosition "Who to follow"
	link p t = clickableText p <$> withTextExtents defaultFont middleSize t

users' :: Locks -> Integer -> SigF View ()
users' ls n = concat <$%> user1' ls `ftraverse` [0 .. n - 1]

user1' :: Locks -> Integer -> SigF View ()
user1' ls n = do
	(a, ln, u) <- waitFor (getUser ls)
	(nm, cr) <- waitFor $ nameCross n ln
	emit $ Image (avatarPosition n) a : view nm <> view cr
	() <$ waitFor (forever $ click nm >> adjust (browse u)) `until` click cr
	user1' ls n

nameCross :: Integer -> T.Text -> ReactF (Clickable, Clickable)
nameCross n t = (<$> withTextExtents defaultFont largeSize t) \wte ->
	(clickableText p wte, cross crossSize $ crossPosition p wte)
	where p = namePosition n

cross :: Integer -> (Integer, Integer) -> Clickable
cross sz (x0, y0) = clickable
	[Line white 4 (x0, y0) (x1, y1), Line white 4 (x1, y0) (x0, y1)]
	(x0', y0') (x1', y1')
	where
	[x1, y1] = (+ sz) <$> [x0, y0]
	[x0', y0'] = subtract crossMergin <$> [x0, y0]
	[x1', y1'] = (+ crossMergin) <$> [x1, y1]

getUser :: Locks -> ReactF (Avatar, T.Text, T.Text)
getUser ls = makeUser <$> getObject1 ls >>= \case
	Left (e, em) -> adjust (raiseError e em) >> getUser ls
	Right (au, l, u) -> getAvatar au >>= \case
		Left (e, em) -> adjust (raiseError e em) >> getUser ls
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
	where bsToImage = (convertRGBA8 <$>) . decodeImage . LBS.toStrict

getObject1 :: Locks -> ReactF Object
getObject1 ls = withLock (lockObjects ls) $ adjust loadJsons >>= \case
	[] -> getObject1'
	o : os -> o <$ adjust (storeJsons os)

getObject1' :: ReactF Object
getObject1' = getObjects >>= \case
	Right (o : os) -> o <$ adjust (storeJsons os)
	Right [] -> do
		adjust $ raiseError EmptyJson "Empty JSON"
		getObject1'
	Left em -> adjust (raiseError NotJson em) >> getObject1'

getObjects :: ReactF (Either String [Object])
getObjects = do
	n <- adjust $ getRandomR (0, userPageMax)
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

twhite :: FontSize -> Position -> T.Text -> View1
twhite = Text white defaultFont

posixSeconds :: BS.ByteString -> Maybe UTCTime
posixSeconds =
	(posixSecondsToUTCTime . fromInteger <$>) . readMaybe . BSC.unpack
