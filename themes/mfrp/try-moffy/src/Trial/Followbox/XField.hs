{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.XField (
	-- * View
	View, View1, view,
	-- * Color
	Color, white, blue ) where

import Control.Moffy.View.XField
import Data.OneOfThem

import Trial.Followbox.ViewType (View(..), View1, Color(..), white, blue)
import Field (Field, clearField, flushField)

---------------------------------------------------------------------------

-- * VIEW
-- * DRAW IMAGE PIXEL

---------------------------------------------------------------------------
-- VIEW
---------------------------------------------------------------------------

view :: Field -> View -> IO ()
view f (View v) = clearField f >> view1 f `mapM_` v >> flushField f

view1 :: Field -> View1 -> IO ()
view1 f = apply $ viewText f >-- viewLine f >-- SingletonFun (viewImage f)
