{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Object where

import Foreign.Ptr
import Stopgap.Data.Ptr

data OTag

newtype O = O (Ptr OTag) deriving Show

class IsPtr o => IsO o where toO :: o -> O
