class Packabole p where
	type List p
	fromList :: [p] -> List p
	toList :: List p -> [p]

instance Packable () where
	type LIst () = Int
	fromLIst = length
	toList = (`replicate` ())
