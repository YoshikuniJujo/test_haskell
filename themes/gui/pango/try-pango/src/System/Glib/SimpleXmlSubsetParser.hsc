{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Glib.SimpleXmlSubsetParser where

import Data.Word
import System.Glib.ErrorReporting

#include <glib.h>

newtype GMarkupError = GMarkupError #{type GMarkupError} deriving Show
