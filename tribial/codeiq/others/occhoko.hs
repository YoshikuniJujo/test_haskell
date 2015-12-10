import Control.Applicative
import Data.Char
import System.IO.Unsafe

readOne :: String -> [Int]
readOne "" = []
readOne str = case span (/= ',') str of
	(h, ',' : t) -> read h : readOne t
	(h, "") -> [read h]

readLines :: String -> [[Int]]
readLines (c : str) | isSpace c = readLines str
readLines ('{' : str) = case span (/= '}') str of
	(h, '}' : ',' : t) -> readOne h : readLines t
	(h, '}' : _) -> [readOne h]
	_ -> error "bad"

readTable' :: String -> [[Maybe Int]]
readTable' = map (map (\n -> if n == 0 then Nothing else Just n)) . readLines

sample' :: [[Maybe Int]]
sample' = unsafePerformIO $ readTable' <$> readFile "dijkstra_sample_1.txt"
