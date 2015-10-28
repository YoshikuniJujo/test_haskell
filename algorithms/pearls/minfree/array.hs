import Data.Array

main :: IO ()
main = interact $ (++ "\n") . show . minfree . map read . lines

minfree :: [Int] -> Int
minfree = search . checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs =
	accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
	where n = length xs
