main :: IO ()
main = interact $ (++ "\n") . show . win . read

data Player = Takahashi | Aoki deriving Show

win :: Integer -> Player
win n | n < 2 = Aoki | n < 6 = Takahashi | otherwise = win $ (n - 2) `div` 4
