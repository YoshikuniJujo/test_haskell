{-# LANGUAGE OverloadedStrings #-}

module DrawBitmap (drwaBitmap) where

import Prelude hiding (null, take, drop, replicate, cycle)
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Data.Bits
import Data.Char
import Control.Monad
import Data.ByteString.Lazy (ByteString, null, unpack, take, drop, replicate, cycle)
import Data.ByteString.Lazy.Char8 ()
import Data.Word
import Data.Int

openWindow :: IO (Display, Window, GC)
openWindow = do
	_ <- initThreads
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let scr = defaultScreen dpy
	root <- rootWindow dpy scr
	win <- createSimpleWindow dpy root 0 0 100 100 1
		(blackPixel dpy scr) (whitePixel dpy scr)
	gc <- createGC dpy win
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	return (dpy, win, gc)

drawBitmap :: Position -> Position -> ByteString -> IO ()
drawBitmap w h dat = do
	(dpy, win, gc) <- openWindow
	flush dpy
	loop' dpy win gc w h dat

loop' :: Display -> Window -> GC -> Position -> Position -> ByteString -> IO ()
loop' dpy win gc w h dat = allocaXEvent $ \e -> do
	nextEvent dpy e
	ev <- getEvent e
	case ev of
		KeyEvent {} -> do
			ch <- fmap (chr . fromEnum) $ keycodeToKeysym dpy
				(ev_keycode ev) 0
			if ch == 'q' then return () else loop dpy win gc
		_ -> do	image dpy win gc w 100 100 0 0 $ take (2500 * 3) dat
			loop dpy win gc

main :: IO ()
main = do
	(dpy, win, gc) <- openWindow
	flush dpy
	loop dpy win gc
--	getLine >> return ()

loop :: Display -> Window -> GC -> IO ()
loop dpy win gc = allocaXEvent $ \e -> do
	nextEvent dpy e
	ev <- getEvent e
--	print ev
	case ev of
		KeyEvent {} -> do
			ch <- fmap (chr . fromEnum) $ keycodeToKeysym dpy
				(ev_keycode ev) 0
			if ch == 'q' then return () else loop dpy win gc
		_ -> do	image dpy win gc 50 100 100 0 0 $ take (2500 * 3) $
				cycle "\xff\x00\xff" -- replicate (2500 * 3) 0
{-
		_ -> do	forM_ [0 .. 50 * 50 - 1] $ \i -> do
				setForeground dpy gc 0xff0000
				drawPoint dpy win gc (100 + i `mod` 50) (100 + i `div` 50)
-}
			loop dpy win gc

-- image :: Display -> Window -> GC -> Position -> Position -> Position -> ByteString -> IO ()
image dpy win gc w x0 y0 x y dat
	| null dat = return ()
	| otherwise = do
		setForeground dpy gc $ rgbToWord32 $ take' 3 dat
		drawPoint dpy win gc (x0 + x) (y0 + y)
		let (x', y') = if x == w - 1 then (0, y + 1) else (x + 1, y)
		image dpy win gc w x0 y0 x' y' $ drop 3 dat

take' :: Int64 -> ByteString -> [Word8]
take' n = unpack . take n

rgbToWord32 :: [Word8] -> Word32
rgbToWord32 rgb = r `shiftL` 16 .|. g `shiftL` 8 .|. b
	where
	[r, g, b] = map fromIntegral rgb
