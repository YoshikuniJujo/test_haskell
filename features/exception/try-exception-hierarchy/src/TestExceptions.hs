{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TestExceptions where

import Control.Exception
import Control.Exception.Hierarchy

import MyException qualified as ME
import MyException qualified

data PutHumanPartialError = PutHumanPartialError deriving Show

exceptionHierarchy (Just ''ME.MyException) $
	ExNode "PutHumanError" [
		ExNode "PutHumanOutOfFieldError" [
			ExType ''PutHumanPartialError ] ]
