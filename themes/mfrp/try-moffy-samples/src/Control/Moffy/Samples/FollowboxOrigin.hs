{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.FollowboxOrigin (
	-- * followbox
	followbox ) where

import Prelude hiding (break, repeat, scanl)

import Control.Arrow ((>>>))
import Control.Monad (void, forever, (<=<))
import Control.Moffy (React, adjust, emit, waitFor, break, indexBy, find, repeat, scanl)
import Control.Moffy.Event.Lock (LockId, newLockId, withLock)
import Control.Moffy.Samples.Event.Random (getRandomR)
import Control.Moffy.Samples.Event.Delete (deleteEvent)
import Control.Moffy.Samples.Viewable.Basic (Position, Color(..), LineWidth)
import Control.Moffy.Samples.Followbox.Event (
	SigF, ReactF,
	httpGet, getTimeZone, beginSleep, checkBeginSleep, endSleep,
	Error(..), raiseError, checkTerminate )
import Control.Moffy.Samples.Followbox.Clickable (
	Clickable, view, click, clickableText, withTextExtents, FontName, FontSize )
import Control.Moffy.Samples.Followbox.ViewType (
	View(..), white, Png(..), VText(..), Line(..), Image(..) )
import Control.Moffy.Samples.Followbox.TypeSynonym (ErrorMessage)
import Data.Type.Set
import Data.Type.Flip ((<$%>), (<*%>))
import Data.OneOfThem
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.Bool
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Time (utcToLocalTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Data.Aeson.KeyMap (toHashMap)
import Text.Read (readMaybe)
import Codec.Picture qualified as P

import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Area

---------------------------------------------------------------------------

-- * PARAMETER LIST
-- 	+ MAX NUMBER OF GITHUB USER
-- 	+ BACKGROUND
-- 	+ FONT
-- 	+ AVATAR, NAME AND CROSS
-- 	+ COLORS
-- * SIG AND REACT
--	+ FOLLOWBOX
--	+ USER VIEW
--	+ GET USERS
--	+ GET OBJECT
-- * MOUSE EVENTS
-- * VIEWS
-- * ERRORS

---------------------------------------------------------------------------
-- PARAMETER LIST
---------------------------------------------------------------------------

-- MAX NUMBER OF GITHUB USER

userPageMax :: Int
userPageMax = 2 ^ (27 :: Int)

-- BACKGROUND

titlePos, refreshPos, resetTimePos :: Position
titlePos = (50, 44); refreshPos = (600, 65)
resetTimePos = (100, 470)

-- FONT

defaultFont :: FontName
defaultFont = "sans"

middleSize, largeSize :: FontSize
middleSize = 30; largeSize = 40

-- AVATAR, NAME AND CROSS

avatarSizeX, avatarSizeY :: Double
(avatarSizeX, avatarSizeY) = (80, 80)

avatarPos, namePos :: Int -> Position
avatarPos n = (100, 120 + 120 * fromIntegral n)
namePos n = (210, 150 + 120 * fromIntegral n)

crossSize :: Double
crossSize = largeSize / 2

crossPos :: Int -> Position
crossPos n = (500, 162 + 120 * fromIntegral n)

crossMergin :: Double
crossMergin = 5

-- COLORS

barColor :: Color
barColor = Color 0x32 0xcd 0x32

nameColor :: Color
nameColor = Color 0x00 0xcd 0x00

---------------------------------------------------------------------------
-- SIG AND REACT
---------------------------------------------------------------------------

-- FOLLOWBOX

followbox :: SigF s View ()
followbox = void $ fieldWithResetTime `break` deleteEvent `break` checkTerminate

fieldWithResetTime :: SigF s View ()
fieldWithResetTime = (<>) <$%> field <*%> resetTime

field :: SigF s View ()
field = do
	rfs <- waitFor $ link refreshPos "Refresh"
	lck <- waitFor $ adjust newLockId
	let	frame = title <> view rfs
	emit frame
	(frame <>) <$%> ((\a b c -> a <> b <> c)
		<$%> (chooseUser 0 <$%> refresh rfs <*%> close lck 0)
		<*%> (chooseUser 1 <$%> refresh rfs <*%> close lck 1)
		<*%> (chooseUser 2 <$%> refresh rfs <*%> close lck 2))
	where
	title = text white largeSize titlePos "Who to follow"
	link p t = clickableText p
		<$> adjust (withTextExtents defaultFont middleSize t)

chooseUser :: Int -> Maybe (Either Int [(Png, T.Text)]) -> Int -> View
chooseUser _ (Just (Left i)) _ = bar $ fromIntegral i
chooseUser n (Just (Right us)) i = userView n (us !! (i `mod` length us))
chooseUser _ Nothing _ = View []

refresh :: Clickable s -> SigF s (Maybe (Either Int [(Png, T.Text)])) ()
refresh rfs = forever do
	emit Nothing
	us <- Just . Left <$%> users
	emit . Just $ Right us
	waitFor . adjust $ click rfs

close :: LockId -> Int -> SigF s Int ()
close lck i = forever do
	emit =<< waitFor
		(withLock lck (adjust $ getRandomR (0, 29) :: ReactF s Int))
	waitFor . clickArea $ crossArea i

resetTime :: SigF s View ()
resetTime = forever do
	emit $ View []
	emit =<< waitFor do
		(t, tz) <- (,) <$> adjust checkBeginSleep <*> adjust getTimeZone
		pure . text white middleSize resetTimePos . T.pack
			$ "Wait until " <> show (utcToLocalTime tz t)
	waitFor $ adjust endSleep

-- USER VIEW

userView :: Int -> (Png, T.Text) -> View
userView n (avt, nm) = image (avatarPos n) avt <> name n nm <> cross n

name :: Int -> T.Text -> View
name n nm = text nameColor largeSize (namePos n) nm

cross :: Int -> View
cross (crossPos -> (l, t)) = line white 4 lt rb <> line white 4 lb rt
	where
	(lt, lb, rt, rb) = ((l, t), (l, b), (r, t), (r, b))
	(r, b) = (l + crossSize, t + crossSize)

crossArea :: Int -> (Position, Position)
crossArea (crossPos -> (l, t)) = ((l', t'), (r', b'))
	where
	(r, b) = (l + crossSize, t + crossSize)
	(l', t') = (l - crossMergin, t - crossMergin)
	(r', b') = (r + crossMergin, b + crossMergin)

-- GET USERS

users :: SigF s Int [(Png, T.Text)]
users = waitFor (mapM ex2 <$> getObjs') >>= err \(unzip -> (avs, nms)) ->
	sequence <$> ssum (avatar `mapM` avs) >>= err (pure . flip zip nms)
	where
	ex2 (toHashMap -> o) = (,)
		<$> extract "avatar_url" o noAvatarAddress
		<*> extract "login" o noLoginName
	ssum = scanl (+) 0
	err = either \e -> waitFor (adjust (uncurry raiseError e)) >> users

extract :: Hashable k => k -> HM.HashMap k Value -> e -> Either e T.Text
extract k o e = case HM.lookup k o of Just (String v) -> Right v; _ -> Left e

avatar :: T.Text -> SigF s Int (Either (Error, ErrorMessage) Png)
avatar url = emit 1 >> waitFor (png . snd <$> adjust (httpGet url))

png :: LBS.ByteString -> Either (Error, String) Png
png = LBS.toStrict >>> convert >>>
	either (Left . (NoAvatar ,)) (Right . Png avatarSizeX avatarSizeY)
	where convert img =
		LBS.toStrict . P.encodePng . P.convertRGB8 <$> P.decodeImage img

bar :: Double -> View
bar p = line barColor 50 (80, 300) (80 + p * 25, 300)

-- GET OBJECT

getObjs' :: ReactF s [Object]
getObjs' = getObjs >>= \case
	Left em -> adjust (raiseError NotJson em) >> getObjs'
	Right [] -> adjust (raiseError EmptyJson "Empty JSON") >> getObjs'
	Right os -> pure os

getObjs :: ReactF s (Either String [Object])
getObjs = do
	n <- adjust $ getRandomR (0, userPageMax)
	(hdr, bdy) <- adjust . httpGet $ api n
	case (rmng hdr, rst hdr) of
		(Just rmn, _) | rmn > (0 :: Int) -> pure $ eitherDecode bdy
		(Just _, Just t) -> sleep t >> getObjs
		(Just _, Nothing) -> err noRateLimitReset >> getObjs
		(Nothing, _) -> err noRateLimitRemaining >> getObjs
	where
	api = ("https://api.github.com/users?since=" <>) . T.pack . show @Int
	rmng = (read . BSC.unpack <$>) . lookup "X-RateLimit-Remaining"
	rst = ut <=< lookup "X-RateLimit-Reset"
	ut = (posixSecondsToUTCTime . fromInteger <$>) . readMaybe . BSC.unpack
	err :: (Error, String) -> ReactF s ()
	err = adjust . uncurry raiseError

sleep :: UTCTime -> ReactF s ()
sleep t = adjust (beginSleep t) >> adjust endSleep

---------------------------------------------------------------------------
-- MOUSE EVENTS
---------------------------------------------------------------------------

clickArea :: (Point, Point) -> ReactF s ()
clickArea ((l, u), (r, d)) = void . adjust
	. find inside $ fst <$%> repeat Mouse.move `indexBy` repeat leftClick
	where inside (x, y) = l <= x && x <= r && u <= y && y <= d

leftClick :: React s (Singleton Mouse.Down) ()
leftClick = clickOn Mouse.ButtonPrimary

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b0 = bool (clickOn b0) (pure ()) . (== b0) =<< Mouse.down

---------------------------------------------------------------------------
-- VIEWS
---------------------------------------------------------------------------

text :: Color -> FontSize -> Position -> T.Text -> View
text c fs p = View . (: []) . expand . Singleton . Text' c defaultFont fs p

line :: Color -> LineWidth -> Position -> Position -> View
line c w p q = View . (: []) . expand . Singleton $ Line' c w p q

image :: Position -> Png -> View
image p = View . (: []) . expand . Singleton . Image' p

---------------------------------------------------------------------------
-- ERRORS
---------------------------------------------------------------------------

noRateLimitRemaining,
	noRateLimitReset, noAvatarAddress, noLoginName :: (Error, String)
noRateLimitRemaining = (NoRateLimitRemaining, "No X-RateLimit-Remaining header")
noRateLimitReset = (NoRateLimitReset, "No X-RateLimit-Reset header")
noAvatarAddress = (NoAvatarAddress, "No Avatar Address")
noLoginName = (NoLoginName, "No Login Name")
