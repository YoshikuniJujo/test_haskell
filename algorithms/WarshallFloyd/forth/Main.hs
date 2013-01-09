test :: [[Int]]
test = [
	[  0, 18,999, 14,999],
	[ 18,  0, 17,999, 16],
	[999, 17,  0, 13,999],
	[ 14,999, 13,  0, 12],
	[999, 16,999, 12,  0]]

wf :: Int -> [[Int]] -> [[Int]]
wf s ps0 = for [0 .. s] ps0 $ \v ps ->
	collect [0 .. s] $ \f ->
		collect [0 .. s] $ \t ->
			min (ps !! f !! t) (ps !! f !! v + ps !! v !! t)

for :: [a] -> b -> (a -> b -> b) -> b
for is s body = foldr body s is

collect :: [a] -> (a -> b) -> [b]
collect = flip map
