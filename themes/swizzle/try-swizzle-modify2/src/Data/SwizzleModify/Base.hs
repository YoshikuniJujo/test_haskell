{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleModify.Base where

import Data.SwizzleModify.Base.TH

concat <$> mkX `mapM` [1 .. 26]
