{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Tools.SampleParser where

import Control.Applicative
import Data.Char

import Internal.Tools.Parse

number :: Parse Integer
number = read <$> some (check isDigit)
