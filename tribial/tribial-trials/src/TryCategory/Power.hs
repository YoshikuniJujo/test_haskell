{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryCategory.Power where

import Control.Arrow
import Data.Bits
import Data.ShortWord

eval :: (Bool, Word6) -> Word3
eval (False, f) = fromIntegral $ f .&. 0b111
eval (True, f) = fromIntegral $ f `shiftR` 3

sampleFunPower :: (Bool, ()) -> Word3
sampleFunPower = \case (False, _) -> 3; (True, _) -> 6

sampleFunPowerGen :: () -> Word6
sampleFunPowerGen _ = 0b110011

sampleFunPower' :: (Bool, ()) -> Word3
sampleFunPower' = eval . second sampleFunPowerGen
