{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy.Samples.Followbox
import Control.Moffy.Samples.Followbox.RunGtkField
import Control.Moffy.Samples.Viewable.Text
import Control.Moffy.Samples.Viewable.Image
import Data.Type.Flip
import Data.Type.Set
import Data.OneOfThem
import Data.Color

import Control.Moffy.Samples.Followbox.ViewType qualified as T
import Control.Moffy.Samples.View
import Control.Moffy.Samples.Viewable.Shape

main :: IO ()
main = runFollowbox "firefox" Nothing $ viewToView <$%> followbox

viewToView :: T.View -> View
viewToView (T.View vs) = View $ (view1ToView1 `apply`) <$> vs

view1ToView1 :: OneOfThemFun (VText :- Line :- Image :- 'Nil) View1
view1ToView1 =
	(\(Line' (T.Color r g b) lw p0 p1) ->
		VLine (RgbWord8 r g b) lw p0 p1) >--
	(\(T.Text' (T.Color r g b) fn fs p txt) ->
		VText (RgbWord8 r g b) fn fs p txt) >-- SingletonFun
	(\(T.Image' p (T.Png w h dt)) -> VImage p w h dt)
