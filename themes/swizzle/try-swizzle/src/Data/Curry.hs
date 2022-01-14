{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Curry where

import Data.Curry.TH

concat <$> crr `mapM` reverse [0 .. 26]
concat <$> unc `mapM` reverse [0 .. 26]
