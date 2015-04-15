maybe2 :: c -> (a -> b -> c) -> Maybe a -> Maybe b -> c
maybe2 _ op (Just x) (Just y) = x `op` y
maybe2 d _ _ _ = d
