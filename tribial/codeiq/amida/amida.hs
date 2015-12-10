import Control.Applicative
import Data.List
import System.Environment

amida1 :: [Int] -> [Int] -> [Int]
amida1 [] [x] = [x]
amida1 [1] [x, y] = [y, x]
amida1 (0 : bs) (x : xs) = x : amida1 bs xs
amida1 (1 : 0 : bs) (x : y : xs) = y : x : amida1 bs xs
amida1 xs bs = error $ "bad amida: " ++ show xs ++ " " ++ show bs

amida :: [[Int]] -> [Int]
amida bs = foldr amida1 [1 .. length (head bs) + 1] bs

readCsv :: String -> [[Int]]
readCsv = map (map read . comma) . lines

comma :: String -> [String]
comma cs = case span (/= ',') cs of
	(w, ',' : r) -> w : comma r
	(w, _) -> [w]

main :: IO ()
main = do
	bss <- readCsv <$> (readFile . head =<< getArgs)
	putStrLn . intercalate "," . map show $ amida bss
