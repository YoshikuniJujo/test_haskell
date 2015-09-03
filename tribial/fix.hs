import Data.Function

fact' :: (Integer -> Integer) -> Integer -> Integer
fact' _ n | n < 1 = 1
fact' f n = n * f (n - 1)

fact :: Integer -> Integer
fact = fix $ \f n -> if n < 1 then 1 else n * f (n - 1)
