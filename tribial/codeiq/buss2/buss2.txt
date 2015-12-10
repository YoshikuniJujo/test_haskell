import Control.Applicative
import Control.Arrow
import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . uncurry fees . input

fees :: [Int] -> [PF] -> Int
fees fes = sum . map (feeD fes) . infant 0 . sortBy (flip compare)

type PF = (P, F)

infant :: Int -> [PF] -> [PF]
infant _ [] = []
infant n (a@(A, _) : ps) = a : infant (n + 2) ps
infant n (i@(I, _) : ps) | n > 0 = infant (n - 1) ps
infant n (p : ps) = p : infant n ps

data P = I | C | A deriving (Show, Read, Eq, Ord)

data F = P | X | N deriving (Show, Read, Eq, Ord)

readF :: Char -> F
readF 'p' = P; readF 'x' = X; readF 'n' = N; readF _ = error "readF: error"

feeD :: [Int] -> PF -> Int
feeD fes = min <$> fee fes <*> fee1 910

fee :: [Int] -> PF -> Int
fee fes pf = sum $ map (flip fee1 pf) fes

fee1 :: Int -> PF -> Int
fee1 fe (p, f) = d $ h fe
	where
	h = case p of A -> id; _ -> half
	d = case f of P -> const 0; X -> discount; _ -> id

half :: Int -> Int
half f = (f + 10) `div` 20 * 10

discount :: Int -> Int
discount f = (f * 56 + 999) `div` 1000 * 10

input :: String -> ([Int], [PF])
input = (map read . split ','
		*** (map (\(p : f : _) -> (read [p], readF f)) . split ',' . tail))
	. span (/= ':')
	where
	split _ [] = [[]]
	split s (x : xs)
		| x == s = [] : split s xs
		| otherwise = (x :) `heading` split s xs
		where heading f (x : xs) = f x : xs
