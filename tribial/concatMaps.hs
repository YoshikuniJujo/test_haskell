concatMap', concatMapRaw, concatMapF :: (a -> [b]) -> [a] -> [b]
concatMap' = (concat .) . map

concatMapRaw f (x : xs) = f x ++ concatMapRaw f xs
concatMapRaw _ _ = []

concatMapF f = foldr ((++) . f) []
