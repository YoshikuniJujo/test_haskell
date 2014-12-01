import System.Environment
import Graphics.UI.WX
import Data.IORef
import Codec.Picture

main :: IO ()
main = do
	fn : _ <- getArgs
	start $ ballsFrame fn

type Picture = [[Bool]]

blank :: IO (IORef Picture)
blank = newIORef . replicate 16 $ replicate 16 False

ballsFrame :: FilePath -> IO ()
ballsFrame fp = do
	pic <- blank
	f <- frame [ text := "hoge" ]
	p <- panel f []
	v <- panel f [
		on keyboard := onKey fp pic f,
		on paint := preview pic ]
	set p [	on keyboard := onKey fp pic f,
		on click := onClick p v pic,
		on clickRight := onClickRight p v pic,
		on paint := onPaint pic,
		on resize := putStrLn "resize" ]
	set f [ layout := row 10 [
			minsize (sz 162 162) $ widget p,
			minsize (sz 16 16) $ widget v ] ]

onKey fp pic f (EventKey (KeyChar 'q') _ _) = do
	p <- readIORef pic
	close f
	writePng fp $ toImage p
onKey _ _ _ k = print k

toImage p = generateImage (\x y -> if p !! y !! x
	then PixelRGB8 0 0 0
	else PixelRGB8 maxBound maxBound maxBound) 16 16

onMouse :: Frame () -> EventMouse -> IO ()
onMouse f (MouseRightDown _ _) = close f
onMouse f (MouseLeftDown p m) = print p >> print m
onMouse _ _ = return ()

-- onPaint :: 
onPaint pic dc viewArea = do
	p <- readIORef pic
	set dc [brushColor := black]
	drawBigPicture 0 dc p
	putStrLn "paint"

onClick pnl v pic p@(Point x y) = do
	putStr "LEFT : "
--	writeIORef pic . replicate 16 $ replicate 16 True
	dot True pic (x `div` 10) (y `div` 10)
	repaint pnl
	repaint v
	print p

onClickRight pnl v pic p@(Point x y) = do
	putStr "RIGHT: "
--	writeIORef pic . replicate 16 $ replicate 16 False
	dot False pic (x `div` 10) (y `div` 10)
	repaint pnl
	repaint v
	print p
	print p

preview pic dc _ = do
	p <- readIORef pic
	drawPicture 0 dc p

drawPicture _ _ [] = return ()
drawPicture y dc (l : ls) = do
	drawLine 0 y dc l
	drawPicture (y + 1) dc ls

drawLine _ _ _ [] = return ()
drawLine x y dc (p : ps) = do
	if p then drawPoint dc (Point x y) [] else return ()
	drawLine (x + 1) y dc ps

drawBigPicture _ _ [] = return ()
drawBigPicture y dc (l : ls) = do
	drawBigLine 0 y dc l
	drawBigPicture (y + 1) dc ls

drawBigLine _ _ _ [] = return ()
drawBigLine x y dc (p : ps) = do
	if p then drawRect dc (Rect (x * 10) (y * 10) 9 9) [] else return ()
	drawBigLine (x + 1) y dc ps

dot :: Bool -> IORef Picture -> Int -> Int -> IO ()
dot b p x y = modifyIORef p (dt b x y)

dt :: Bool -> Int -> Int -> Picture -> Picture
dt b x y pic = take y pic ++ [l'] ++ drop (y + 1) pic
	where
	l = pic !! y
	l' = take x l ++ [b] ++ drop (x + 1) l
