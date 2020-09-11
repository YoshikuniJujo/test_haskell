{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.Internal (
	-- * Type
	Key(..) ) where

import Data.Word (Word64)

newtype Key = Key Word64 deriving (Show, Eq, Ord)
