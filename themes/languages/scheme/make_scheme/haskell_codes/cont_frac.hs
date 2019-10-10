contFrac, contFracIter :: Fractional f => (Int -> f) -> (Int -> f) -> Int -> f
contFrac n d k = cf 1
	where
	cf i | i > k = 0
	cf i = n i / (d i + cf (i + 1))

contFracIter n d k = cf k 0
	where
	cf i r | i < 1 = r
	cf i r = cf (i - 1) (n i / (d i + r))

napier :: Fractional f => Int -> f
napier n = case n `mod` 3 of
	2 -> fromIntegral $ (n `div` 3 + 1) * 2
	_ -> 1

tanCf :: Fractional f => f -> Int -> f
tanCf x = contFrac
	(\i -> if i == 1 then x else - x ^ 2)
	(subtract 1 . (* 2) . fromIntegral)
