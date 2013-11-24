{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Applicative
import System.IO

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile fp = (Just <$> readFile fp)
	`catch` \(_ :: SomeException) -> return Nothing

eitherReadFile :: FilePath -> IO (Either String String)
eitherReadFile fp = (Right <$> readFile fp)
	`catch` \(e :: SomeException) -> return $ Left $ show e

fldiv0 :: FilePath -> IO Int
fldiv0 fp = do
	cnt <- readFile fp
	return $ length cnt `div` 0

printDivFileLen :: FilePath -> IO ()
printDivFileLen fp = do
	cnt <- readFile fp
	print $ 100 `div` length cnt

class Some a where
	this :: a

instance Some Bool

whichError :: Int
whichError = error "urk" + (1 `div` 0)

readFirstLine :: FilePath -> IO String
readFirstLine fp = bracket (openFile fp ReadMode) hClose hGetLine

printDiv0 :: IO ()
printDiv0 = do
	r <- try $ return $ 1 `div` 0 :: IO (Either ArithException Int)
	case r of
		Left e -> print e
		Right n -> print n

printDiv0' :: IO ()
printDiv0' = do
	r <- try $ evaluate $ 1 `div` 0 :: IO (Either ArithException Int)
	case r of
		Left e -> print e
		Right n -> print n

-- data MyException = MyException
