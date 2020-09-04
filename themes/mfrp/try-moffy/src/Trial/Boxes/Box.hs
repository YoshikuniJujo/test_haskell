{-# LANGUAGE TypeApplications, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Box (
	-- * Box
	Box(..), Rect(..), Color(..) ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Control.Moffy.Event.Mouse (Point)
import Control.Moffy.Handle.GtkField

import Graphics.Gtk.Cairo

data Box = Box Rect Color deriving Show
data Rect = Rect { leftup :: Point, rightdown :: Point  } deriving Show
data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

instance Storable Box where
	sizeOf _ = sizeOf @Double undefined * 4 + sizeOf @Int undefined
	alignment _ = max (alignment @Int undefined) (alignment @Double undefined)
	peek p = do
		[l, u, r, d] <- peekArray 4 (castPtr p)
		c <- peek . castPtr $ p `plusPtr` (sizeOf @Double undefined * 4)
		pure $ Box (Rect (l, u) (r, d)) (toEnum c)
	poke p (Box (Rect (l, u) (r, d)) c) = do
		pokeArray (castPtr p) [l, u, r, d]
		poke (castPtr $ p `plusPtr` (sizeOf @Double undefined * 4)) $ fromEnum c

instance Drawable Box where
	draw cr b@(Box (Rect (l_, u_) (r, d)) c) = do
		print b
		uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
		cairoRectangle cr l u w h
		cairoStrokePreserve cr
		cairoFill cr
		where
		l = min l_ r
		u = min u_ d
		w = abs $ l_ - r
		h = abs $ u_ - d

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

colorToRgb :: Color -> (Double, Double, Double)
colorToRgb Red = (1, 0, 0)
colorToRgb Green = (0, 1, 0)
colorToRgb Blue = (0, 0, 1)
colorToRgb Yellow = (1, 1, 0)
colorToRgb Cyan = (0, 1, 1)
colorToRgb Magenta = (1, 0, 1)
