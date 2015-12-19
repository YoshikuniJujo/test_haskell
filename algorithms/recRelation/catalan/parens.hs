parens :: Int -> Integer
parens 0 = 1
parens 1 = 1
parens n = sum [ parens h * parens t | h <- [0 .. n - 1], let t = n - h - 1 ]

parensL :: [Integer]
parensL = map parens [0 ..]
