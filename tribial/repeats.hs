
repeatRaw, repeatI :: a -> [a]
repeatRaw x = x : repeatRaw x

repeatI = iterate id

replicateRaw, replicateT :: Int -> a -> [a]
replicateRaw n x | n > 0 = x : replicateRaw (n - 1) x
replicateRaw _ _ = []

replicateT n = take n . repeat

cycleRaw, cycleC :: [a] -> [a]
cycleRaw xs = xs ++ cycleRaw xs

cycleC = concat . repeat
