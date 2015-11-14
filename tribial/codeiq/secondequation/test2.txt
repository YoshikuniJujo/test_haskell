main :: IO ()
main = interact $ (++ "\n") . show . (scanl1 (+) (map cnt [0 ..]) !!) . read

cnt :: Integral n => n -> n
cnt n = sum (map ((n ^ 2 `div`) . (* 4)) [1 .. n `div` 2]) * 2 - (n `div` 2) ^ 2
