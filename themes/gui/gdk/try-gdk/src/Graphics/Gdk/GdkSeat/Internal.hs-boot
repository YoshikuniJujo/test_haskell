{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkSeat.Internal where

import Foreign.Ptr

newtype GdkSeat = GdkSeat (Ptr GdkSeat)
