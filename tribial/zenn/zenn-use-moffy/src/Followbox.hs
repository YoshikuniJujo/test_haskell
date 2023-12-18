{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox where

import Prelude hiding (break, repeat, scanl)
import Control.Arrow ((>>>))
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
