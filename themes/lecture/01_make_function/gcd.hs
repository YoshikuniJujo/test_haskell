import Prelude hiding (gcd)

gcd m 0 = m
gcd m n = gcd n (m `mod` n)

extend rs m 0 = (rs, m)
extend rs m n = extend (m `div` n : rs) n (m `mod` n)

ex m n = let (rs, 1) = extend [] m n in
	(mkj (reverse rs) !! (length rs - 1), mkk (reverse rs) !! (length rs - 1))

--
-- gcd (13, 5)
-- 13 = 2 * 5 + 3	=> 3 = 13 - 2 * 5
-- 5 = 1 * 3 + 2	=> 2 = 5 - 1 * 3
-- 3 = 1 * 2 + 1	=> 1 = 3 - 1 * 2
-- 2 = 2 * 1
--
-- gcd (m, n)
-- m = r1 * n + s1	=> s1 = m - r1 * n	(j1, k1)
-- n = r2 * s1 + s2	=> s2 = n - r2 * s1	(j2, k2)
-- s1 = r3 * s2 + s3	=> s3 = s1 - r3 * s2	(j3, k3)
-- s2 = r4 * s3 + s4	=> s4 = s2 - r4 * s3	(j4, k4)
-- s3 = r5 * s4 + s5	=> s5 = s3 - r5 * s4	(j5, k5)
-- s4 = r6 * s5 + 1	=> 1 =  s4 - r6 * s5	(j6, k6)
--
-- gcd (13, 11)
-- 13 = 1 * 11 + 2	=> 2 = 13 - 1 * 11
-- 11 = 5 * 2 + 1	=> 1 = -5 * 13 + (1 + 5) * 11
-- 2 = 2 * 1
--
--
-- j0 = 0           , k0 = 1
-- j1 = 1           , k1 = -r1
-- j2 = j0 - r2 * j1, k2 = k0 - r2 * k1
-- j3 = j1 - r3 * j2, k3 = k1 - r3 * k2,
-- j4 = j2 - r4 * j3, k4 = k2 - r4 * k3,

mkj (_ : rs) = js
	where
	js = 0 : 1 : zipWith3 (\j1 j2 r -> j1 - r * j2) js (tail js) rs

mkk (r0 : rs) = ks
	where
	ks = 1 : - r0 : zipWith3 (\k1 k2 r -> k1 - r * k2) ks (tail ks) rs
