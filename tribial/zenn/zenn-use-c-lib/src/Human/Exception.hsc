{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.Exception where

import Control.Exception
import Control.Exception.Hierarchy
import Data.Typeable
import Data.Word

#include <human.h>

data PutHumanPartialError = PutHumanPartialError deriving (Typeable, Show)
data PutHumanOffscreenError = PutHumanOffscreenError deriving (Typeable, Show)

data PutHumanUnknownError = PutHumanUnknownError #{type HmPutHumanResult}
	deriving (Typeable, Show)

exceptionHierarchy Nothing (
	ExNode "PutHumanError" [
		ExNode "PutHumanOutOfFieldError" [
			ExType ''PutHumanPartialError,
			ExType ''PutHumanOffscreenError ],
		ExType ''PutHumanUnknownError ] )
