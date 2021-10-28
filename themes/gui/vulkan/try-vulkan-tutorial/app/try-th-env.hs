{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ThEnv

main :: IO ()
main = print $(lookupCompileEnvExp "NDEBUG")
