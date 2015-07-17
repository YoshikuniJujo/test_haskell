import Control.Arrow
import Data.List
import Data.Function
import Data.Char
import System.Environment
import Numeric

main :: IO ()
main = getArgs >>= readFile . head >>= putStr . unlines . map (
		flip (showFFloat (Just 2)) ""
			. last . last . (\(Matrix m) -> m)
			. head . dropWhile (not . finished)
			. iterate step
			. toMatrix
			. uncurry calcAll
			. (map cust . split ',' ***
				map prod . filter (not . null) . split ',' . tail)
			. span (/= ';')
	) . lines

split :: Eq a => a -> [a] ->[[a]]
split _ [] = [[]]
split s (x : xs)
	| x == s = [] : split s xs
	| True = (x :) `heading` split s xs
	where heading f (x : xs) = f x : xs

data Cust = Cust String deriving Show

cust :: String -> Cust
cust = Cust . map toLower . filter isAlpha

data Prod = Prod String deriving Show

prod :: String -> Prod
prod = Prod . map toLower . filter isAlpha

calcAll :: [Cust] -> [Prod] -> [[Double]]
calcAll cs ps = map (\c -> map (calc c) ps) cs

calc :: Cust -> Prod -> Double
calc (Cust c) (Prod p)
	| even lp = m * 1.5 * fromIntegral (length $ filter (`elem` vowels) c)
	| True = m * fromIntegral (length $ filter (`elem` consonants) c)
	where
	m = if lc `gcd` lp == 1 then 1 else 1.5
	lc = length c
	lp = length p

vowels, consonants :: [Char]
vowels = "aeiouy"
consonants = ['a' .. 'z'] \\ vowels

toMatrix :: [[Double]] -> Matrix
toMatrix dss = Matrix $
	map ((++ [1]) . concat) (onesRow dss) ++
	map ((++ [1]) . concat) (onesCol dss) ++
	[map negate (concat dss) ++ [0]]

onesRow1 :: [[a]] -> Int -> [[Double]]
onesRow1 xss i = map (map (const 0)) u ++ [map (const 1) h] ++ map (map (const 0)) l
	where (u, h : l) = splitAt i xss

onesRow :: [[a]] -> [[[Double]]]
onesRow xss = map (onesRow1 xss) [0 .. length xss - 1]

onesCol1 :: [[a]] -> Int -> [[Double]]
onesCol1 xss i = map to1 xss
	where
	to1 xs = let (b, _ : f) = splitAt i xs in
		map (const 0) b ++ [1] ++ map (const 0) f

onesCol :: [[a]] -> [[[Double]]]
onesCol xss = map (onesCol1 xss) [0 .. length (head xss) - 1]

data Matrix = Matrix [[Double]] deriving Show

sample :: Matrix
sample = Matrix [
	[1, 1, 0, 0, 1],
	[1, 0, 1, 0, 1],
	[0, 1, 0, 1, 1],
	[0, 0, 1, 1, 1],
	[-9, -10, -2, -4, 0] ]

pivotj :: Matrix -> Int
pivotj (Matrix m) = minimumBy (compare `on` (l !!)) [0 .. length l - 1]
	where l = last m

pivoti :: Matrix -> Int -> Int
pivoti (Matrix m) j = minimumBy (compare `on` \k -> last (m !! k) / m !! k !! j)
	$ filter ((> 0) . (\k -> m !! k !! j)) [0 .. length m - 2]

pivot :: Matrix -> (Int, Int)
pivot m = let j = pivotj m; i = pivoti m j in (i, j)

eliminate :: Matrix -> Int -> Int -> Matrix
eliminate (Matrix m) i j =
	Matrix $ map (eliminate1 j h) u ++ [h] ++ map (eliminate1 j h) l
	where
	(u, h : l) = splitAt i m

eliminate1 :: Int -> [Double] -> [Double] -> [Double]
eliminate1 j r2 r1 = zipWith (-) r1 $ map (* (r1 !! j / r2 !! j)) r2

step :: Matrix -> Matrix
step m = uncurry (eliminate m) $ pivot m

finished :: Matrix -> Bool
finished (Matrix m) = all (>= 0) $ last m
