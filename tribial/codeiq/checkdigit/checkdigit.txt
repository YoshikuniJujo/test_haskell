import Control.Applicative
import Data.Char

main :: IO ()
main = interact $ unlines . map (showR . check) . lines

data Result = Error | NG | OK | CS Int deriving Show

showR :: Result -> String
showR (CS n) = show n; showR r = show r

check :: String -> Result
check s	| any (not . isDigit) s = Error
	| otherwise = cd 0 (map (read . (: "")) s) $ [6, 5 .. 2] ++ [7, 6 .. 2]
	where
	cd c [] [] = CS $ fn c
	cd c [d] [] | fn c == d = OK | otherwise = NG
	cd c (d : ds) (n : ns) = cd ((c + d * n) `mod` 11) ds ns
	cd _ _ _ = Error
	fn c | c <= 1 = 0 | otherwise = 11 - c
