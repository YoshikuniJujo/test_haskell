{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.OpenUnionValue where

import Data.Maybe

import OpenUnionValue

findValue :: Member a as => [UnionValue as] -> Maybe a
findValue = listToMaybe . mapMaybe prj
