{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Moffy.Samples.Followbox
import Control.Moffy.Samples.Followbox.RunGtkField
import Data.Type.Flip
import Data.OneOfThem
import Data.Color

import Control.Moffy.Samples.Followbox.ViewType qualified as T
import Control.Moffy.Samples.View
import Control.Moffy.Samples.Viewable.Shape

main :: IO ()
main = runFollowbox "firefox" Nothing $ viewToView <$%> followbox

viewToView :: T.View -> View
viewToView (T.View vs) = View $ view1ToView1 <$> vs

view1ToView1 :: T.View1 -> View1
view1ToView1 v1 = case project v1 of
	Just (Line' (T.Color r g b) lw p0 p1) -> VLine (RgbWord8 r g b) lw p0 p1
	Nothing -> case project v1 of
		Just (T.Text' (T.Color r g b) fn fs p txt) ->
			VText (RgbWord8 r g b) fn fs p txt
		Nothing -> case project v1 of
			Just (T.Image' p (T.Png w h dt)) -> VImage p w h dt
			Nothing -> NotImplemented
