{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Curry where

import Data.Curry.TH

concat <$> crr `mapM` [0 .. 26]
concat <$> unc `mapM` [0 .. 26]
