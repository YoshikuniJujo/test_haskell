{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.List qualified as L
import System.Directory
import System.FilePath

main :: IO ()
main = putStrLn
	. (\(t, j) -> show t ++ "/" ++ show (t + j))
	. (length *** length)
	. span' (".ts" `L.isSuffixOf`) (".js" `L.isSuffixOf`) =<< listDirectoryRec "./src"

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' d = ((d </>) <$>) <$> listDirectory d

listDirectoryRec :: FilePath -> IO [FilePath]
listDirectoryRec d = do
	isf <- doesFileExist d
	isd <- doesDirectoryExist d
	case (isf, isd) of
		(True, False) -> pure [d]
		(False, True) -> concat <$> (mapM listDirectoryRec =<< listDirectory' d)
		_ -> error "bad"

span' :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
span' p q = \case
	[] -> ([], [])
	x : xs	| p x -> (x :) `first` span' p q xs
		| q x -> (x :) `second` span' p q xs
		| otherwise -> span' p q xs
