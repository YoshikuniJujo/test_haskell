{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Bit = O | I deriving Show

toI :: Maybe Integer -> Maybe Integer -> [Bit] -> (Maybe Integer, [Bit])
toI mn _ [] = (mn, [])
toI Nothing Nothing (O : bs) = toI Nothing (Just 0) bs
toI Nothing Nothing (I : bs) = toI (Just 0) Nothing bs
toI Nothing (Just 0) (O : bs) = toI Nothing (Just $ - 1) bs
toI Nothing (Just 0) (I : bs) = toI (Just $ - 1) (Just 0) bs
toI (Just 0) Nothing (O : bs) = toI (Just 0) (Just 1) bs
toI (Just 0) Nothing (I : bs) = toI (Just 1) Nothing bs
toI Nothing (Just mx) (O : bs) = toI Nothing (Just $ mx * 2) bs
toI Nothing (Just mx) (I : bs) = toI (Just $ mx * 2) (Just mx) bs
toI (Just mn) Nothing (O : bs) = toI (Just mn) (Just $ mn * 2) bs
toI (Just mn) Nothing (I : bs) = toI (Just $ mn * 2) Nothing bs
toI (Just mn) (Just mx) (O : bs)
	| mn + 2 <= mx = toI (Just mn) (Just $ (mn + mx) `div` 2) bs
toI (Just mn) (Just mx) (I : bs)
	| mn + 2 <= mx = toI (Just $ (mn + mx) `div` 2) (Just mx) bs
toI mn _ bs = (mn, bs)

fromI :: Maybe Integer -> Maybe Integer -> Integer -> [Bit]
fromI Nothing Nothing n
	| n < 0 = O : fromI Nothing (Just 0) n
	| otherwise = I : fromI (Just 0) Nothing n
fromI Nothing (Just 0) n
	| n < - 1 = O : fromI Nothing (Just $ - 1) n
	| otherwise = I : fromI (Just $ - 1) (Just 0) n
fromI (Just 0) Nothing n
	| n < 1 = O : fromI (Just 0) (Just 1) n
	| otherwise = I : fromI (Just 1) Nothing n
fromI Nothing (Just mx) n
	| n < mx * 2 = O : fromI Nothing (Just $ mx * 2) n
	| otherwise = I : fromI (Just $ mx * 2) (Just mx) n
fromI (Just mn) Nothing n
	| n < mn * 2 = O : fromI (Just mn) (Just $ mn * 2) n
	| otherwise = I : fromI (Just $ mn * 2) Nothing n
fromI (Just mn) (Just mx) n
	| mn + 2 > mx = []
	| n < (mn + mx) `div` 2 = O : fromI (Just mn) (Just $ (mn + mx) `div` 2) n
	| otherwise = I : fromI (Just $ (mn + mx) `div` 2) (Just mx) n
