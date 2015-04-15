import Control.Applicative
import Control.Arrow
import Data.List
import Data.Ratio

main :: IO ()
main = interact $ unlines . map (unwords . map show . egyptian)
	. map (((%) <$> (!! 0) <*> (!! 1)) . map read . words)
	. (\(ln : lns) -> take (read ln) lns)
	. lines

egyptian :: Rational -> [Integer]
egyptian r
	| numerator r == 1 = let
		(m, n) = ((+ 1) &&& (*) <$> id <*> (+ 1)) $ denominator r in
		[m, n]
	| otherwise = unfoldr uncons r

uncons :: Rational -> Maybe (Integer, Rational)
uncons r | r <= 0 = Nothing
uncons r = Just (m, r - 1 % m)
	where
	(n, d) = numerator &&& denominator $ r
	m = d `div` n + if d `mod` n == 0 then 0 else 1
