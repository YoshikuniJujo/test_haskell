{-# LANGUAGE ViewPatterns #-}

addLookup env ((`lookup` env) -> Just val1) ((`lookup` env) -> Just val2) =
	val1 + val2
addLookup _ _ _ = 0
