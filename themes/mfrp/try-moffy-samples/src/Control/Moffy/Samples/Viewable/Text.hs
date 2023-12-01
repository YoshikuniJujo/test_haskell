{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Viewable.Text (VText(..)) where

import Control.Moffy.Samples.Followbox.Event.CalcTextExtents (FontName, FontSize)
import Data.Text (Text)

import Data.Type.Set

import Control.Moffy.Samples.Viewable.Basic

---------------------------------------------------------------------------

-- TEXT

data VText = Text' Color FontName FontSize Position Text deriving Show
numbered [t| VText |]
