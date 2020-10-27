{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Gdk.PointsAndRectangles
import Graphics.Gdk.Types

main :: IO ()
main = do
	putStrLn . ("gdkRectangleIntersect ...: " ++) . show
		=<< gdkRectangleIntersect (GdkRectangle 0 0 100 100) (GdkRectangle 0 0 100 100)
	putStrLn . ("gdkRectangleIntersect ...: " ++) . show
		=<< gdkRectangleIntersect (GdkRectangle 50 50 100 100) (GdkRectangle 0 0 100 100)
	putStrLn . ("gdkRectangleIntersect ...: " ++) . show
		=<< gdkRectangleIntersect (GdkRectangle 00 00 100 100) (GdkRectangle 100 100 100 100)
	putStrLn . ("gdkRectangleUnion ...: " ++) . show
		=<< gdkRectangleUnion (GdkRectangle 50 50 100 100) (GdkRectangle 100 100 100 50)
	putStrLn . ("gdkRectangleEqual ...: " ++) . show
		=<< gdkRectangleEqual (GdkRectangle 50 50 100 100) (GdkRectangle 50 50 100 100)
	putStrLn . ("gdkRectangleEqual ...: " ++) . show
		=<< gdkRectangleEqual (GdkRectangle 50 50 100 100) (GdkRectangle 50 100 100 100)
