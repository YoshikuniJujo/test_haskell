cm :: (a -> [b]) -> (b -> [c]) -> a -> [c]
-- cm g h = concatMap h . g
cm g h = concat . map h . g

{-

cm g h = concat . map h . g

g `cm` (h `cm` k) == (g `cm` h) `cm` k

g `cm` (h `cm` k)
=> concat . map (h `cm` k) . g
=> concat . map (concat . map k . h) . g
=> concatMap (concatMap k . h) . g
=> \x -> concatMap (concatMap k . h) (g x)
=> \x -> concatMap (\y -> concatMap k (h y)) (g x)

(g `cm` h) `cm` k
=> concat . map k . (concat . map h . g)
=> concatMap k . (concatMap h . g) 

concatMap (concatMap k . h) . g == concatMap k . concatMap h . g
concatMap (concatMap k . h) (g x) == concatMap k (concatMap h (g x))
concatMap (concatMap k . h) xs == concatMap k (concatMap h xs)

concatMap f (x : xs) = f x ++ concatMap f xs
concatMap _ _ = []

concatMap (concatMap k . h) (x : xs)
=> concatMap k (h x) ++ concatMap (concatMap k . h) xs

concatMap k (concatMap h (x : xs))
=> concatMap k (h x ++ concatMap h xs)
...
=> concatMap k (h x) ++ concatMap k (concatMap h xs)

concatMap g (xs ++ ys) == concatMap g xs ++ concatMap g ys

concatMap g ((x : xs) ++ ys) 
=> concatMap g (x : (xs ++ ys))
=> g x ++ concatMap g (xs ++ ys)

concatMap g (x : xs) ++ concatMap g ys
=> g x ++ concatMap g xs ++ concatMap g ys

(x : xs) ++ ys == x : (xs ++ ys)

concatMap (concatMap f . g) == concatMap f . concatMap g

-}
