{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS
import Data.SwizzleModify.Base qualified as B

import Try.TH

xy :: (	Swz.Swizzle1 (SwzS.Y s b), Swz.Swizzle2 s,
	SwzS.SwizzleSet1 (SwzS.Y s b) a, SwzS.SwizzleSet2 s b ) =>
	(Swz.X (SwzS.Y s b) -> a, Swz.Y s -> b) -> s -> SwzS.X (SwzS.Y s b) a
xy (mx, my) = B.x mx . B.y my
