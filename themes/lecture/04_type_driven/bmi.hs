bmi :: Double -> Double -> Double
bmi h w = w / (h / 100) ^ 2

isObese :: Double -> Double -> Bool
isObese h w = bmi h w >= 25
