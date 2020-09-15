{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.Text (VText(..)) where

import Control.Moffy.Event.CalcTextExtents (FontName, FontSize)
import Data.Text (Text)

import Trial.Followbox.TypeSynonym (Position)

import Data.Type.Set

import Trial.Followbox.Color

---------------------------------------------------------------------------

-- TEXT

data VText = Text' Color FontName FontSize Position Text
numbered [t| VText |]
