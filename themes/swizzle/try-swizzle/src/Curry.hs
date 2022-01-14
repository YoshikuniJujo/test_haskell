{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Curry where

import Curry.TH

concat <$> crr `mapM` [0 .. 26]
concat <$> unc `mapM` [0 .. 26]
