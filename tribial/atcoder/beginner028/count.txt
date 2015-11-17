import Control.Applicative
import Data.Bool
import Data.List

main :: IO ()
main = interact $
	(++ "\n") . unwords . map show . getZipList . count (ZipList "ABCDEF")

count :: ZipList Char -> String -> ZipList Int
count as "" = const 0 <$> as
count as (c : cs) = (+) . bool 0 1 . (== c) <$> as <*> count as cs
