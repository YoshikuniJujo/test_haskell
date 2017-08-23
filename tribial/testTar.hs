{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main, toByteString) where

import Data.Maybe
import Data.List
import Data.Tree
import System.IO
import System.IO.Temp
import System.Environment (getArgs)
import System.Directory
import Foreign.Ptr
import Foreign.Marshal

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

import Tar

-- MAIN FUNCTIONS

main :: IO ()
main = mainEx . head =<< getArgs

mainEx :: FilePath -> IO ()
mainEx tfp = do
	h <- openFile tfp ReadMode
	cnt <- BS.readFile tfp
	withTmpDir "hoge" $ do
		hUntar h
		directoryTree "." >>= putStr . showTree 0
		tar "tmp.tar" . filter (not . isPrefixOf ".")
			=<< getDirectoryContents "."
		getDirectoryContents "." >>= print
		cnt2 <- BS.readFile "tmp.tar"
		print $ BS.take (BS.length cnt2) cnt == cnt2
		print $ diff cnt cnt2
--		print $ BS.take 512 cnt == BS.take 512 cnt2
		print $ BS.length cnt
		print $ BS.length cnt2
		print $ BS.take 512 cnt
		print $ BS.take 512 cnt2
--		copyFile "tmp.tar" "../tmp.tar"

data DF = Directory | File deriving Show

checkDF :: FilePath -> IO DF
checkDF fp = do
	f <- doesFileExist fp
	d <- doesDirectoryExist fp
	case (f, d) of
		(True, False) -> return File
		(False, True) -> return Directory
		_ -> error "bad filepath"

showTree :: Show a => Int -> Tree a -> String
showTree idt (Node x ts) =
	replicate idt ' ' ++ show x ++ "\n" ++ concatMap (showTree $ idt + 4) ts

directoryTree :: FilePath -> IO (Tree FilePath)
directoryTree fp = do
	df <- checkDF fp
	case df of
		File -> do
			cnt <- readFile fp
			return $ Node (fp ++ ": " ++ take 20 cnt) []
		Directory -> do
			cd <- getCurrentDirectory
			setCurrentDirectory fp
			Node fp
				<$> (mapM directoryTree . filter (not . isPrefixOf ".")
					=<< getDirectoryContents ".")
				<* setCurrentDirectory cd

withTmpDir :: String -> IO a -> IO a
withTmpDir n act = do
	cd <- getCurrentDirectory
	withTempDirectory "." n $ \fp -> do
		setCurrentDirectory fp
		act <* setCurrentDirectory cd

toByteString :: Ptr a -> Int -> IO BS.ByteString
toByteString p n = BSI.create n $ \b -> copyBytes b (castPtr p) n

diff :: BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)]
diff bs cs = map BS.unzip . myMaybes
	$ BS.zipWith (\w v -> if w == v then Nothing else Just (w, v)) bs cs

myMaybes :: [Maybe a] -> [[a]]
myMaybes (Just x : Nothing : ms) = [x] : myMaybes ms
myMaybes (Just x : ms) = case myMaybes ms of
	xs : xss -> (x : xs) : xss
	[] -> [[x]]
myMaybes (Nothing : ms) = myMaybes ms
myMaybes [] = []
