diffRecip :: Double -> Double -> Maybe Double
diffRecip x y = case x - y of
	0 -> Nothing
	d	| d > 0 -> Just $ recip d
		| otherwise -> Just $ recip (- d)
