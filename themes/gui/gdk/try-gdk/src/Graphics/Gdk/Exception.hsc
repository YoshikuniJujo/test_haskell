{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Exception (
	-- * GDK EXCEPTION
	GdkException, gdkExceptionFromException, gdkExceptionToException,
	-- * GDK EXCEPTION MEMBERS
	GdkInitFail(..), gdkInitFail,
	GdkNoDefaultDisplay(..), GdkCannotOpenDisplay(..),
	GdkIndexOutOfRange(..) ) where

import Control.Exception
import Control.Exception.Hierarchy

data GdkInitFail = GdkInitFail deriving Show
data GdkNoDefaultDisplay = GdkNoDefaultDisplay deriving Show
data GdkCannotOpenDisplay = GdkCannotOpenDisplay deriving Show
data GdkIndexOutOfRange = GdkIndexOutOfRange deriving Show

exceptionHierarchy Nothing $ ExNode "GdkException" [
	ExType ''GdkInitFail,
	ExType ''GdkNoDefaultDisplay, ExType ''GdkCannotOpenDisplay,
	ExType ''GdkIndexOutOfRange ]

gdkInitFail :: IO a
gdkInitFail = throw GdkInitFail
