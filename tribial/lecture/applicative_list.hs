import Control.Applicative

app :: [a -> b] -> [a] -> [b]
app fs xs = (`map` xs) `concatMap` fs
-- app fs xs = concatMap (\f -> map f xs) fs
-- app fs xs = concat $ map (\f -> map f xs) fs
-- app fs xs = concat $ (`map` xs) `map` fs

unit :: [()]
unit = pure ()

(.**) :: [a] -> [b] -> [(a, b)]
xs .** ys = pure (,) <*> xs <*> ys
