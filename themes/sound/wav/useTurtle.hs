{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.X11.Turtle
import CheckWav

main :: IO ()
main = do
	f <- openField
	onkeypress f (return . (/= 'q'))
	t <- newTurtle f
	penup t
	uncurry (goto t) $ head points
	pendown t
	mapM_ (uncurry $ goto t) $ tail points
	hideturtle t
	waitField f

points :: [(Double, Double)]
points = [-250, -249.993 .. 250] `zip`
	map ((/ 128) . fromIntegral) (drop 0 wavData2)

main2 :: IO ()
main2 = do
	f <- openField
	onkeypress f (return . (/= 'q'))
	t <- newTurtle f
	penup t
	uncurry (goto t) $ head points2
	pendown t
	mapM_ (uncurry $ goto t) $ tail points2
	hideturtle t
	waitField f

points2 :: [(Double, Double)]
points2 = [-250, -249.6 .. 250] `zip`
	map ((/ 128) . fromIntegral) (drop 6500 wavData2)
