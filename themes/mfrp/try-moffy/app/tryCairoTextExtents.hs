{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import qualified Data.Text as T

import Trial.TryCalcTextExtents

main :: IO ()
main = runTryCalcTextExtentsGtk . T.pack . unwords =<< getArgs
