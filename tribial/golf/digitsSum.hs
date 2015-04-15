import Data.Char

main = interact sm

sm s@[_] = s
sm n = sm . show $ sum (map (subtract 48 . fromIntegral . ord) n)
