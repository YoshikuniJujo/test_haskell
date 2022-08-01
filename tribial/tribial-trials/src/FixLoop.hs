{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FixLoop where

import Control.Monad.Fix
import Data.Bool

fixLoopFoo :: [String] -> IO ()
fixLoopFoo = fix \loop -> \case [] -> pure (); m : ms -> putStrLn m >> loop ms
