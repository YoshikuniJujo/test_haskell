
calc, calc' :: [(Bool, Integer)] -> Integer
calc ((True, n) : bns) = n * calc bns
calc ((_, n) : bns) = n + calc bns
calc [] = 0

calc1 :: (Bool, Integer) -> Integer -> Integer
calc1 (True, n) m = n * m
calc1 (_, n) m = n + m

calc' = foldr calc1 0
