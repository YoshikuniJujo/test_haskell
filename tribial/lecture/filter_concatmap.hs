
filterC, filterC' :: (a -> Bool) -> [a] -> [a]
filterC p = concatMap $ \x -> concatMap (\_ -> [x]) $ if p x then [()] else []

filterC' p = concat . map (\x -> map (const x) $ if p x then [()] else [])

filterC'' p = concat . map (\x -> if p x then [x] else [])

filterC''' p = concatMap $ \x -> if p x then [x] else []
