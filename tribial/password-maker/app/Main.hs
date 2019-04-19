{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Exception
import Data.Bool
import System.IO
import System.Environment
import System.Directory
import System.FilePath
import Crypto.Hash

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Base58.BitcoinFlavor as B58

import Lib

main :: IO ()
main = do
	cmd : args <- getArgs
	case cmd of
		"setWords" -> setWords
		"checkWords" -> bool (putStrLn "BAD!") (putStrLn "OK!")
			=<< checkWords =<< inputWords
		"makePassword" -> case args of
			[url] -> putStrLn =<< makePassword (BSC.pack url)
			_ -> error "bad args"
		_ -> putStrLn "no such command"


setWords :: IO ()
setWords = do
	ws <- take 8 <$> randomWords
	mapM_ T.putStrLn ws
	writeHash $ hash @BS.ByteString @SHA256 . E.encodeUtf8 $ T.concat ws

getTopDirectory :: IO FilePath
getTopDirectory = (</> ".password-maker") <$> getHomeDirectory

getHashFile :: IO FilePath
getHashFile = (</> "words-hash.txt") <$> getTopDirectory

writeHash :: Digest a -> IO ()
writeHash d = do
	createDirectoryIfMissing False =<< getTopDirectory
	hf <- getHashFile
	renameFileIfExist hf $ hf <.> "bak"
	writeFile hf (show d <> "\n")

renameFileIfExist :: FilePath -> FilePath -> IO ()
renameFileIfExist src dst =
	bool (return ()) (renameFile src dst) =<< doesFileExist src

checkWords :: [T.Text] -> IO Bool
checkWords ws = do
	let	h = T.pack . (<> "\n") . show . hash @BS.ByteString @SHA256
			. E.encodeUtf8 $ T.concat ws
	h0 <- T.readFile =<< getHashFile
	return $ h == h0

inputWords :: IO [T.Text]
inputWords = (8 `timesDo`) $ inputWord =<< getWords

timesDo :: Int -> IO a -> IO [a]
n `timesDo`  act
	| n < 1 = return []
	| otherwise = (:) <$> act <*> (n - 1) `timesDo` act

makePassword :: BS.ByteString -> IO String
makePassword url = do
	ws <- inputWords
	b <- checkWords ws
	if b	then return . take 12 . B58.encode . (<> url)
			. BA.convert . hash @BS.ByteString @SHA256
			. E.encodeUtf8 $ T.concat ws
		else error "wrong words"

inputWord :: [T.Text] -> IO T.Text
inputWord ws = do
	w <- withEcho False T.getLine
	maybe (inputWord ws) return =<< checkWord ws w

checkWord :: [T.Text] -> T.Text -> IO (Maybe T.Text)
checkWord ws w
	| w `elem` ws = putStrLn "" >> return (Just w)
	| otherwise = putStrLn "no such word" >> return Nothing

withEcho :: Bool -> IO a -> IO a
withEcho e act = do
	old <- hGetEcho stdin
	bracket_ (hSetEcho stdin e) (hSetEcho stdin old) act
