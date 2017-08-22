{-# LANGUAGE LambdaCase #-}

safeRecip = \case
	0 -> Nothing
	x -> Just $ 1 / x

safeRecip' 0 = Nothing
safeRecip' x = Just $ 1 / x
