{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Moffy.Samples.Followbox
import Control.Moffy.Samples.Followbox.RunGtkField
import Data.Type.Flip
import Data.OneOfThem
import Data.Color

import Trial.Followbox.ViewType qualified as T
import Control.Moffy.Samples.View
import Control.Moffy.Viewable.Shape

main :: IO ()
main = runFollowbox "firefox" Nothing $ viewToView <$%> followbox

viewToView :: T.View -> View
viewToView (T.View vs) = View $ view1ToView1 <$> vs

view1ToView1 :: T.View1 -> View1
view1ToView1 v1 = case project v1 of
	Just (Line' (T.Color r g b) lw p0 p1) -> VLine (RgbWord8 r g b) lw p0 p1
	Nothing -> NotImplemented