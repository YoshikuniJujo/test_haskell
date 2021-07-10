{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Inits1 where

import Data.List.NonEmpty hiding (inits)

import {-# SOURCE #-} Inits

inits1 :: [a] -> [NonEmpty a]
inits1 = \case [] -> []; (x : xs) -> (x :|) <$> inits xs
