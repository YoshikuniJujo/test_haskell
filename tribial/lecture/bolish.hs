import Data.List

bolish, bolish' :: [(Bool, Integer)] -> Integer
bolish [] = 0
bolish ((False, n) : ns) = n + bolish ns
bolish ((_, n) : ns) = n * bolish ns

bolish' = foldr (\(b, n) -> (if b then (*) else (+)) n) 0

rbolishIter :: Integer -> [(Bool, Integer)] -> Integer
rbolishIter s [] = s
rbolishIter s ((False, n) : ns) = rbolishIter (s + n) ns
rbolishIter s ((_, n) : ns) = rbolishIter (s * n) ns

rbolish, rbolish' :: [(Bool, Integer)] -> Integer
rbolish = rbolishIter 0

rbolish' = foldl' (\s (b, n) -> (if b then (*) else (+)) s n) 0
