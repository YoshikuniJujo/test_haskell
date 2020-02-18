{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FieldAndMonadicFrp where

import GuiEv
import ButtonEvent

mouseButton :: ButtonNumber -> Maybe MouseBtn
mouseButton Button1 = Just MLeft
mouseButton Button2 = Just MMiddle
mouseButton Button3 = Just MRight
mouseButton _ = Nothing
