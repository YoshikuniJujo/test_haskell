{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.Image (Image(..), Png(..)) where

import Data.Type.Set
import qualified Data.ByteString as BS

import Trial.Followbox.TypeSynonym (Position)

data Image = Image' Position Png
data Png = Png { pngWidth :: Double, pngHeight :: Double, pngData :: BS.ByteString }
	deriving Show
numbered [t| Image |]
