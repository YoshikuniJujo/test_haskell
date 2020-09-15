{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.Shape (Line(..)) where

import Data.Type.Set

import Trial.Followbox.TypeSynonym (LineWidth)
import Trial.Followbox.Basic

data Line = Line' Color LineWidth Position Position
numbered [t| Line |]
