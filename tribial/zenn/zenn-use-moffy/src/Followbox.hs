{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox where

import Prelude hiding (break, repeat, scanl)
import Control.Monad (void, forever, (<=<))
import Control.Moffy
import Control.Moffy.Event.Lock
import Control.Moffy.Samples.Event.Random
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Area
import Control.Moffy.Samples.Viewable.Basic
import Control.Moffy.Samples.Followbox.Event
import Control.Moffy.Samples.Followbox.Clickable
import Control.Moffy.Samples.Followbox.ViewType
import Control.Moffy.Samples.Followbox.TypeSynonym (ErrorMessage)
import Data.Type.Set
import Data.Type.Flip ((<$%>), (<*%>))
import Data.OneOfThem
import Data.Hashable
import Data.HashMap.Strict qualified as HM
import Data.Bool
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Data.Time (utcToLocalTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson (Object, Value(..), eitherDecode)
import Data.Aeson.KeyMap (toHashMap)
import Text.Read (readMaybe)
import Codec.Picture qualified as P

userMax :: Int
userMax = 2 ^ (27 :: Int)

titlePos, refreshPos, resetTimePos :: Position
titlePos = (50, 44)
refreshPos = (600, 65)
resetTimePos = (100, 470)

defaultFont :: FontName
defaultFont = "sans"

middleSize, largeSize :: FontSize
middleSize = 30; largeSize = 40

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

barColor :: Color
barColor = Color 0x32 0xcd 0x32

nameColor :: Color
nameColor = Color 0x00 0xcd 0x00

noRateLimitRemaining,
	noRateLimitReset, noAvatarAddress, noLoginName :: (Error, String)
noRateLimitRemaining = (NoRateLimitRemaining, "No X-RateLimit-Remaining header")
noRateLimitReset = (NoRateLimitReset, "No X-RateLimit-Reset header")
noAvatarAddress = (NoAvatarAddress, "No Avatar Address")
noLoginName = (NoLoginName, "No Login Name")

text :: Color -> FontSize -> Position -> T.Text -> View
text c fs p = View . (: []) . expand . Singleton . Text' c defaultFont fs p

line :: Color -> LineWidth -> Position -> Position -> View
line c w p q = View . (: []) . expand . Singleton $ Line' c w p q

image :: Position -> Png -> View
image p = View . (: []) . expand . Singleton . Image' p

getObjs :: ReactF s (Either String [Object])
getObjs = do
	n <- adjust $ getRandomR (0, userMax)
	(hdr, bdy) <- adjust . httpGet $ api n
	case (rmng hdr, rst hdr) of
		(Just rmn, _) | rmn > (0 :: Int) -> pure $ eitherDecode bdy
		(Just _, Just t) -> sleep t >> getObjs
		(Just _, Nothing) -> err noRateLimitReset >> getObjs
		(Nothing, _) -> err noRateLimitRemaining >> getObjs
	where
	api = ("http://api.github.com/users?since=" <>) . T.pack . show @Int
	rmng = (read . BSC.unpack <$>) . lookup "X-RateLimit-Remaining"
	rst = ut <=< lookup "X-RateLimit-Reset"
	ut = (posixSecondsToUTCTime . fromInteger <$>) . readMaybe . BSC.unpack
	err = adjust . uncurry raiseError
	sleep t = adjust (beginSleep t) >> adjust endSleep

multiLine :: Show a => Int -> a -> View
multiLine n = text (Color 0 0 0) 15 (15, 15) . T.pack . unlines . sep . show
	where
	sep "" = []
	sep cs = take n cs : sep (drop n cs)

getObjs' :: ReactF s [Object]
getObjs' = getObjs >>= \case
	Left em -> adjust (raiseError NotJson em) >> getObjs'
	Right [] -> adjust (raiseError EmptyJson "Empty JSON") >> getObjs'
	Right os -> pure os

avatar :: T.Text -> SigF s Int (Either (Error, ErrorMessage) Png)
avatar url = emit 1 >> waitFor (epng . convert . snd <$> adjust (httpGet url))
	where
	epng = either
		(Left . (NoAvatar ,))
		(Right . Png avatarSizeX avatarSizeY)
	convert img = LBS.toStrict . P.encodePng . P.convertRGB8
		<$> P.decodeImage (LBS.toStrict img)
