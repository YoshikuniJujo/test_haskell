{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Fctl (

	fctlPoss', Fctlable(..)

	) where

import Data.Word
import Data.Color
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header
import Data.Apng

newtype FrameNumber = FrameNumber Int deriving Show

data Body
	= BodyNull | BodyEnd | BodyFdatEnd
	| BodyFctl Fctl | BodyRgba [Rgba Double] deriving Show

data BodyGray
	= BodyGrayNull | BodyGrayEnd | BodyGrayFdatEnd
	| BodyGrayFctl Fctl | BodyGrayPixels [Word8] deriving Show

class Fctlable a where getFctl :: a -> Maybe Fctl

fctlPoss' :: Header.Header -> Fctl -> [[(Int, Int)]]
fctlPoss' hdr fctl = Header.calcPoss' hdr (fctlWidth fctl) (fctlHeight fctl)
