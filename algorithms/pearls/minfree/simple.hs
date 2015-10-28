main :: IO ()
main = interact $ (++ "\n") . show . minfree . map read . lines

minfree :: [Int] -> Int
minfree xs = head ([0 ..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us
