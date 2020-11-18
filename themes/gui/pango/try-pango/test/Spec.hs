{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Types

main :: IO ()
main = do
	print . pangoExtentsToPixelsInclusive $ PangoRectangle 2000 2000 2000 2000
	print . pangoExtentsToPixelsNearest $ PangoRectangle 2000 2000 2000 2000
