{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Hex where

import Prelude qualified as P
import Prelude hiding (readFile, writeFile)

import Data.Maybe
import Data.List qualified as L
import Data.Word
import Data.ByteString qualified as BS
import Numeric

newtype H = H { unH :: BS.ByteString }

instance Show H where show = toString

instance Read H where readsPrec _ = (: []) . (, "") . fromString

toString :: H -> String
toString (H bs) = ($ "") . foldr (.) id . map wordToHexString $ BS.unpack bs

wordToHexString :: Word8 -> ShowS
wordToHexString w = \s ->
	let	s' = showHex w ""
		l = length s' in
		replicate (2 - l) '0' ++ s' ++ s

fromString :: String -> H
fromString = H . BS.pack . L.unfoldr (listToMaybe . readHexWord)

readHexWord :: String -> [(Word8, String)]
readHexWord "" = []
readHexWord (c0 : c1 : cs) = do
	(w, "") <- readHex [c0, c1]
	pure (w, cs)
readHexWord _ = error "bad"

readFile :: FilePath -> IO H
readFile = (fromString . head . lines <$>) . P.readFile

readFileList :: FilePath -> IO [H]
readFileList = ((fromString <$>) . lines <$>) . P.readFile

writeFile :: FilePath -> H -> IO ()
writeFile fp = P.writeFile fp . (++ "\n") . toString

writeFileList :: FilePath -> [H] -> IO ()
writeFileList fp = P.writeFile fp . unlines . (toString <$>)
