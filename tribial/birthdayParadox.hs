import Numeric

foo :: Int -> Int -> Rational
foo i n = 1 - product (fromIntegral <$> [2 ^ i - (fromIntegral n :: Integer) + 1 .. 2 ^ i]) / (2 ^ i) ^ n

bar :: Int -> Int -> String
bar i n = showFFloat (Just 3) (fromRational $ foo i n * 100) . ('%' :) $ ""

baz :: Int -> Int -> IO ()
baz i n = putStrLn $ bar i n
