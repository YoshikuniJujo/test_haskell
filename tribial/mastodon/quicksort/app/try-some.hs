{-# LANGUAGE TypeApplications #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import qualified Data.List as L
import System.Directory

main :: IO ()
main = print =<< next

next :: IO Int
next = (+ 1) . maximum
	. (read @Int . takeWhile (/= '.') . drop 5 <$>)
	. filter ("trial" `L.isPrefixOf`) <$> getDirectoryContents "graph"
