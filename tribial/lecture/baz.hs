
ffibs, tffibs, ttffibs :: [Integer]
ffibs@(_ : tffibs@(_ : ttffibs)) =
	0 : 0 : 1 : zipWith3 (\x y z -> x + y + z) ffibs tffibs ttffibs

bifs, tbifs :: [Integer]
-- bifs@(_ : tbifs) = 0 : 1 : zipWith (\x y -> 2 * x + y) bifs tbifs
bifs@(_ : tbifs) = 0 : 1 : zipWith ((+) . (2 *)) bifs tbifs
