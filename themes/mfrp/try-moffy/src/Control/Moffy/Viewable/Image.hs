{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Viewable.Image (Image(..), Png(..)) where

import Data.Type.Set
import qualified Data.ByteString as BS

import Control.Moffy.Viewable.Basic (Position)

data Image = Image' Position Png deriving Show
data Png = Png { pngWidth :: Double, pngHeight :: Double, pngData :: BS.ByteString }
	deriving Show
numbered [t| Image |]
