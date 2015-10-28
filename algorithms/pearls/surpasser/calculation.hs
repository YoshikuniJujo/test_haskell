msc :: Ord a => [a] -> Int
msc xs = maximum [ scount z zs | z : zs <- tails xs ]

scount :: Ord a => a -> [a] -> Int
scount x xs = length $ filter (x <) xs

tails :: [a] -> [[a]]
tails [] = []
tails (x : xs) = (x : xs) : tails xs

table :: Ord a => [a] -> [(a, Int)]
table xs = [ (z, scount z zs) | z : zs <- tails xs ]

-- tails (xs ++ ys) == map (++ ys) (tails xs) ++ tails ys
--
-- table (xs ++ ys)
-- =>	[ (z, scount z zs) | z : zs <- tails (xs ++ ys) ]
-- =>	[ (z, scount z zs) | z : zs <- map (++ ys) (tails xs) ++ tails ys ]
-- =>	[ (z, scount z (zs ++ ys)) | z : zs <- tails xs ] ++
-- 	[ (z, scount z zs) | z : zs <- tails ys ]
-- =>	[ (z, scount z zs + scount z ys) | z : zs <- tails xs ] ++
-- 	[ (z, scount z zs) | z : zs <- tails ys ]
-- =>	[ (z, c + scount z (map fst (table ys))) | (z, c) <- table xs ] ++ table ys

join :: Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join txs tys = [(z, c + tcount z tys) | (z, c) <- txs ] ++ tys

tcount :: Ord a => a -> [(a, b)] -> Int
tcount z tys = scount z (map fst tys)

-- tcount z tys
-- =>	length (filter (z <) (map fst tys))
-- =>	length (map fst (filter ((z <) . fst) tys))
-- =>	length (filter ((z <) . fst) tys)
-- =>	length (dropWhile ((z >=) . fst) tys)

tcount' :: Ord a => a -> [(a, b)] -> Int
tcount' z tys = length (dropWhile ((z >=) . fst) tys)

-- join' txs tys = [ (x, c + tcount x tys) | (x, c) <- txs ] `merge` tys
