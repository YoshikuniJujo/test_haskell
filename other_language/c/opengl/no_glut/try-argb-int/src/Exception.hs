{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Exception where

import Control.Exception
import Data.Typeable

import Control.Exception.Hierarchy

data XOpenDisplayError = XOpenDisplayError String deriving (Typeable, Show)

exceptionHierarchy Nothing (
	ExNode "XcbError" [
		ExType ''XOpenDisplayError ] )
