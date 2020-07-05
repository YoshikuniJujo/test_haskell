{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment
import System.Directory

main :: IO ()
main = do
	fp : _ <- getArgs
	renameFile (fp ++ ".rpl") fp
