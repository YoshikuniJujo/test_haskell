-- Language: Haskell
-- Answer:   104

import Data.List

main :: IO ()
main = interact $ (++ "\n") . show
	. length . filter ((== 1) . length) . group . sort
	. concatMap triangleLines . map readTriangle . lines

data Triangle = Triangle Int Int Int deriving Show

data Line = Line Int Int deriving Show

instance Eq Line where
	Line p q == Line r s = p == r && q == s || p == s && q == r

instance Ord Line where
	Line p q <= Line r s
		| max p q < max r s = True
		| max p q == max r s = min p q <= min r s
		| otherwise = False

triangleLines :: Triangle -> [Line]
triangleLines (Triangle a b c) = [Line a b, Line b c, Line c a]

readTriangle :: String -> Triangle
readTriangle = ((\(a, b, c) -> Triangle a b c) . read . ('(' :) . (++ ")"))
