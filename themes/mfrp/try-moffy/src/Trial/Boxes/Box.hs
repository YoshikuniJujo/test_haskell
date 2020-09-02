{-# LANGUAGE TypeApplications, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Box (
	-- * Box
	Box(..), Rect(..), Color(..), NewBoxes(..) ) where

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
	sizeOf _ = sizeOf @Int undefined * 5
	alignment _ = alignment @Int undefined
	peek p = do
		[l, u, r, d, c] <- peekArray 5 (castPtr p)
		pure $ Box (Rect (fromIntegral l, fromIntegral u) (fromIntegral r, fromIntegral d)) (toEnum c)
	poke p (Box (Rect (l, u) (r, d)) c) = pokeArray (castPtr p)
		[fromIntegral l, fromIntegral u, fromIntegral r, fromIntegral d, fromEnum c]

newtype NewBoxes = NewBoxes [Box] deriving (Show, Drawable)

instance Storable NewBoxes where

instance Drawable Box where
	draw cr b@(Box (Rect (l_, u_) (r, d)) _) = do
		print b
		cairoRectangle cr l u w h
		cairoStroke cr
		where
		l = fromIntegral $ min l_ r
		u = fromIntegral $ min u_ d
		w = fromIntegral $ abs $ l_ - r
		h = fromIntegral $ abs $ u_ - d
