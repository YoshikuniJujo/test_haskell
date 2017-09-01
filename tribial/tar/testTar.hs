{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Tree (Tree(..))
import Data.Bool (bool)
import System.IO (IOMode(..), stderr, openFile)
import System.IO.Temp (withTempDirectory)
import System.Directory (
	doesDirectoryExist, doesFileExist,
	getDirectoryContents, getCurrentDirectory, setCurrentDirectory )
import Test.HUnit (Test(..), runTestText, putTextToHandle, assertEqual)

import qualified Data.ByteString as BS

import Tar (tar, hUntar)

-- MAIN FUNCTIONS

main :: IO ()
main = (() <$) . runTestText (putTextToHandle stderr False) $ TestList [
	mkTest "files/tar/sample.tar",
	mkTest "files/tar/simple.tar",
	mkTest "files/tar/nested.tar",
	mkTest "files/tar/simpleNested.tar",
	mkTest "files/tar/longName.tar",
	mkTest "files/tar/longDirAndFile.tar" ]

mkTest :: FilePath -> Test
mkTest tf = TestCase $ do
	org <- BS.readFile tf
	h <- openFile tf ReadMode
	withinTempDirectory "testTar" $ do
		hUntar h
		putStr . showTree 0 =<< directoryTree "."
		tar "new.tar" . nfilter dotPath =<< getDirectoryContents "."
		new <- BS.readFile "new.tar"
		(\em -> assertEqual em org new) $
			"diff: " ++ show (diff org new) ++ "\n" ++
			"original length: " ++ show (BS.length org) ++ "\n" ++
			"new file length: " ++ show (BS.length new)

-- DIRECTORY

withinTempDirectory :: String -> IO a -> IO a
withinTempDirectory dnt act = do
	cd <- getCurrentDirectory
	withTempDirectory "." dnt $ \td -> do
		setCurrentDirectory td
		act <* setCurrentDirectory cd

directoryTree :: FilePath -> IO (Tree FilePath)
directoryTree fp0 = do
	d <- doesDirectoryExist fp0
	f <- doesFileExist fp0
	case (d, f) of
		(True, False) -> do
			cd <- getCurrentDirectory
			setCurrentDirectory fp0
			Node fp0
				<$> (mapM directoryTree . nfilter dotPath
					=<< getDirectoryContents ".")
				<* setCurrentDirectory cd
		(False, True) -> do
			cnt <- readFile fp0
			return $ Node (fp0 ++ ": " ++ take 20 cnt) []
		_ -> error $ fp0 ++ ": no such file or directory"

dotPath :: FilePath -> Bool
dotPath = \case ('.' : _) -> True; _ -> False

-- TOOLS

diff :: BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)]
diff bs = map BS.unzip . sm
	. BS.zipWith (\w v -> bool (Just (w, v)) Nothing $ w == v) bs
	where
	sm (Just x : Nothing : ms) = [x] : sm ms
	sm (Just x : ms) = case sm ms of
		xs : xss -> (x : xs) : xss
		[] -> [[x]]
	sm (Nothing : ms) = sm ms
	sm [] = []

showTree :: Show a => Int -> Tree a -> String
showTree idt (Node x ts) = replicate idt ' ' ++ show x ++ "\n" ++
	concatMap (showTree $ idt + 4) ts

nfilter :: (a -> Bool) -> [a] -> [a]
nfilter = filter . (not .)
