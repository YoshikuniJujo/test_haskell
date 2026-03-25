{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Text.IO qualified as T
import Tools
import Pair

main :: IO ()
main = do
	Right (sc, pb) <- pair'
	withFileCreationMask mask $ T.writeFile "nsec" sc
	T.writeFile "npub" pb
