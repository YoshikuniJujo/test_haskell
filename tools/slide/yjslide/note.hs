module Main where

import Graphics.X11.Turtle
import Data.Char
import Data.IORef
import System.IO.Unsafe
import Control.Monad

fontSize :: Double
fontSize = 20
lineTop, lineLeft :: Double
lineTop = 60
lineLeft = 40

history :: IORef [Char]
history = unsafePerformIO $ newIORef []

popHistory :: IO (Maybe Char)
popHistory = do
	ca <- readIORef history
	case ca of
		c : cs -> do
			writeIORef history cs
			return $ Just c
		_ -> return Nothing

main :: IO ()
main = do
	f <- openField
	topleft f
	t <- newTurtle f
	penup t
	goto t lineLeft lineTop
	onkeypress f $ \c -> do
		print c
		modifyIORef history (c :)
		drawChar t c
		return $ c /= '\ESC'
	waitField f

drawChar :: Turtle -> Char -> IO ()
drawChar t '\b' = do
	_ <- popHistory
	mc <- popHistory
	print mc
	case mc of
		Just '\r' -> replicateM_ 4 $ undo t
		Just _ -> replicateM_ 2 $ undo t
		_ -> return ()
drawChar t '\r' = do
	right t 90
	forward t $ fontSize * 3 / 2
	left t 90
	setx t lineLeft
drawChar t c = do
	write t "KochiGothic" fontSize [c]
	forward t $ getCharWidth c

getCharWidth :: Char -> Double
getCharWidth c
	| isAscii c = fontSize * 3 / 4
	| otherwise = fontSize * 5 / 4
