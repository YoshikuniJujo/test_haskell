{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Exception where

import Control.Exception
import Control.Exception.Hierarchy

data GdkInitFail = GdkInitFail deriving Show

exceptionHierarchy Nothing $ ExNode "GdkException" [
	ExType ''GdkInitFail
	]

gdkInitFail :: IO a
gdkInitFail = throw GdkInitFail
