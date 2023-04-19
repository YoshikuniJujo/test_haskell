{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main, fooBar) where

import Lib

import Data.TypeLevel.Bool qualified as TBool
import Data.TypeLevel.Maybe qualified as TMaybe

main :: IO ()
main = someFunc

foo :: TMaybe.M 'Nothing
foo = TMaybe.N

bar :: TMaybe.M ('Just Int)
bar = TMaybe.J 123

fooBar :: Bool -> String
fooBar b = TBool.b @Show b foo bar show
