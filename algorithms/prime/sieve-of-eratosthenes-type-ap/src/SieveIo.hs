{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module SieveIo (sieveIo) where

import Data.Array.IO

import SieveGen

sieveIo :: Int -> IO [Int]
sieveIo = sieveM @IOArray
