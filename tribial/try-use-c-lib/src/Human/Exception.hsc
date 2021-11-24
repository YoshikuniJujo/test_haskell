{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.Exception where

import Control.Exception
import Control.Exception.Hierarchy
import Data.Typeable
import Data.Word

#include <human.h>

data DrawHumanPartialError = DrawHumanPartialError deriving (Typeable, Show)
data DrawHumanOffscreenError = DrawHumanOffscreenError deriving (Typeable, Show)

data DrawHumanUnknownError = DrawHumanUnknownError #{type HmDrawHumanResult}
	deriving (Typeable, Show)

exceptionHierarchy Nothing (
	ExNode "DrawHumanError" [
		ExNode "DrawHumanOutOfFieldError" [
			ExType ''DrawHumanPartialError,
			ExType ''DrawHumanOffscreenError ],
		ExType ''DrawHumanUnknownError ] )
