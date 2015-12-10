import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Word
import Data.Function

main :: IO ()
-- main = interact $ (show :: (Tree (Either Int Char), [Word8]) -> String) . huffman . read
main = interact $ (show :: ((Tree (Either Int Char), (Int, [Word8])), Int) -> String)
	. (id &&& length . snd . snd) . huffman . read

data Tree a = Branch Int (Tree a) (Tree a) | Leaf Int a deriving Show

weight :: Tree a -> Int
weight (Branch w _ _) = w
weight (Leaf w _) = w

huffman :: Ord a => [a] -> (Tree a, (Int, [Word8]))
huffman xs = (id &&& toWords . (`toCode` xs) . mkTable) . mkTree $ toLeafs xs

toLeafs :: Ord a => [a] -> [Tree a]
toLeafs = sortBy (compare `on` weight)
	. map (uncurry Leaf . (length &&& head)) . group . sort

add :: Tree a -> Tree a -> Tree a
add t1 t2 = Branch (weight t1 + weight t2) t1 t2

mkTree :: [Tree a] -> Tree a
mkTree [t] = t
mkTree (t1 : t2 : ts) = mkTree $ insertBy (compare `on` weight) (add t1 t2) ts

data OI = O | I deriving (Show, Enum)

toWord8 :: [OI] -> Word8
toWord8 [] = 0
toWord8 (oi : ois) = fromIntegral (fromEnum oi) + 2 * toWord8 ois

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)

toWords :: [OI] -> (Int, [Word8])
toWords = (,) <$> length <*> map toWord8 . groupN 8

mkTable :: Tree a -> [(a, [OI])]
mkTable (Leaf _ x) = [(x, [])]
mkTable (Branch _ t1 t2) =
	map ((O :) `second`) (mkTable t1) ++
	map ((I :) `second`) (mkTable t2)

toCode :: Eq a => [(a, [OI])] -> [a] -> [OI]
toCode t = concat . catMaybes . map (`lookup` t)
