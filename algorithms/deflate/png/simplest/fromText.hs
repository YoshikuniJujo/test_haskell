{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import Text.Read (readMaybe)
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Numeric (readHex)

import Codec (encode, image)

main :: IO ()
main = do
	fp : _ <- getArgs
	maybe (return ()) (BS.writeFile (replaceExtension fp "png"))
		. (encode . uncurry (uncurry image) <$>)
		. input . lines =<< readFile fp

input :: [String] -> Maybe ((Int, Int), [BS.ByteString])
input (s : i) = (, map bs i) <$> readMaybe ('(' : s ++ ")")
	where
	bs (d1 : d2 : ds) = fst (head $ readHex [d1, d2]) `BS.cons` bs ds
	bs _ = ""
input _ = Nothing
