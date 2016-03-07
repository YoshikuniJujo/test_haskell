app :: [a -> b] -> [a] -> [b]
app fs xs = (`map` xs) `concatMap` fs
-- app fs xs = concatMap (\f -> map f xs) fs
