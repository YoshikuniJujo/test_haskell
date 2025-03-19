module Tools where

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
