{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Applicative
import Data.Foldable

z :: [a] -> ZipList a; unz :: ZipList a -> [a]; cycleZ :: [a] -> ZipList a
(z, unz, cycleZ) = (ZipList, getZipList, z . cycle)

main :: IO ()
main = traverse_ (either @Int print putStrLn) . take 100 . unz $ z [1 ..]
	<**> cycleZ [Left, Left, const $ Right "Fizz"]
	<**> cycleZ [id, id, id, id,
		either (const Right) ((Right .) . (<>)) `flip` "Buzz"]
