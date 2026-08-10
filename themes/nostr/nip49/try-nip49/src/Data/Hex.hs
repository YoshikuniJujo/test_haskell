{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
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

fromString :: String -> H
fromString = H . BS.pack . L.unfoldr (listToMaybe . readHexWord)

readHexWord :: String -> [(Word8, String)]
readHexWord = \case
	"" -> []
	(c0 : c1 : cs) -> (, cs) . fst <$> readHex [c0, c1]
	_ -> error "bad"

toString :: H -> String
toString (H bs) = ($ "") . foldr (.) id . map hex $ BS.unpack bs
	where
	hex w = let s = showHex w "" in ((replicate (2 - length s) '0' ++ s) ++)

readFile :: FilePath -> IO H
readFile = (fromString . head . lines <$>) . P.readFile

readFileList :: FilePath -> IO [H]
readFileList = ((fromString <$>) . lines <$>) . P.readFile

writeFile :: FilePath -> H -> IO ()
writeFile fp = P.writeFile fp . (++ "\n") . toString

writeFileList :: FilePath -> [H] -> IO ()
writeFileList fp = P.writeFile fp . unlines . (toString <$>)
