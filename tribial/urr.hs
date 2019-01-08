{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Bit = O | I deriving Show

readBits :: String -> [Bit]
readBits "" = []
readBits ('0' : bs) = O : readBits bs
readBits ('1' : bs) = I : readBits bs
readBits _ = error "bad bits"

fromBits :: String -> Maybe Double
fromBits = toDoubleSign . readBits

toDoubleSign :: [Bit] -> Maybe Double
toDoubleSign [] = error "no bit"
toDoubleSign (O : bs) = Just $ toDouble Nothing Nothing bs
toDoubleSign (I : bs) = toDoubleNeg Nothing Nothing bs

toDoubleNeg :: Maybe Int -> Maybe Int -> [Bit] -> Maybe Double
toDoubleNeg mn _ [] = negate . (2 **) . fromIntegral <$> mn
toDoubleNeg Nothing Nothing (O : bs) = toDoubleNeg Nothing (Just 0) bs
toDoubleNeg Nothing Nothing (I : bs) = toDoubleNeg (Just 0) Nothing bs
toDoubleNeg Nothing (Just 0) (O : bs) = toDoubleNeg Nothing (Just 1) bs
toDoubleNeg Nothing (Just 0) (I : bs) = toDoubleNeg (Just 1) (Just 0) bs
toDoubleNeg Nothing (Just mx) (O : bs) = toDoubleNeg Nothing (Just $ mx * 2) bs
toDoubleNeg Nothing (Just mx) (I : bs) = toDoubleNeg (Just $ mx * 2) (Just mx) bs
toDoubleNeg (Just 0) Nothing (O : bs) = toDoubleNeg (Just 0) (Just $ - 1) bs
toDoubleNeg (Just 0) Nothing (I : bs) = toDoubleNeg (Just $ - 1) Nothing bs
toDoubleNeg (Just mn) Nothing (O : bs) = toDoubleNeg (Just mn) (Just $ mn * 2) bs
toDoubleNeg (Just mn) Nothing (I : bs) = toDoubleNeg (Just $ mn * 2) Nothing bs
toDoubleNeg (Just mn) (Just mx) (O : bs)
	| mn >= mx + 2 = toDoubleNeg (Just mn) (Just $ (mn + mx) `div` 2) bs
toDoubleNeg (Just mn) (Just mx) (I : bs)
	| mn >= mx + 2 = toDoubleNeg (Just $ (mn + mx) `div` 2) (Just mx) bs
toDoubleNeg (Just mn) (Just mx) bs = Just
	$ toDoubleRest (- 2 ** fromIntegral mn) (- 2 ** fromIntegral mx) bs


toDouble :: Maybe Int -> Maybe Int -> [Bit] -> Double
toDouble Nothing _ [] = 0
toDouble (Just mn) _ [] = 2 ** fromIntegral mn
toDouble Nothing Nothing (O : bs) = toDouble Nothing (Just 0) bs
toDouble Nothing Nothing (I : bs) = toDouble (Just 0) Nothing bs
toDouble Nothing (Just 0) (O : bs) = toDouble Nothing (Just $ - 1) bs
toDouble (Just 0) Nothing (I : bs) = toDouble (Just 1) Nothing bs
toDouble (Just 0) Nothing (O : bs) = toDouble (Just 0) (Just 1) bs
toDouble Nothing (Just mx) (O : bs) = toDouble Nothing (Just $ mx * 2) bs
toDouble (Just mn) Nothing (O : bs) = toDouble (Just mn) (Just $ mn * 2) bs
toDouble (Just mn) Nothing (I : bs) = toDouble (Just $ mn * 2) Nothing bs
toDouble (Just mn) (Just mx) (O : bs)
	| mn + 2 <= mx = toDouble (Just mn) (Just $ (mn + mx) `div` 2) bs
toDouble (Just mn) (Just mx) (I : bs)
	| mn + 2 <= mx = toDouble (Just $ (mn + mx) `div` 2) (Just mx) bs
toDouble (Just mn) (Just mx) bs =
	toDoubleRest (2 ** fromIntegral mn) (2 ** fromIntegral mx) bs
toDouble Nothing (Just 0) (I : bs) = toDouble (Just $ - 1) (Just 0) bs
toDouble Nothing (Just mx) (I : bs) = toDouble (Just $ mx * 2) (Just mx) bs

toDoubleRest :: Double -> Double -> [Bit] -> Double
toDoubleRest mn _ [] = mn
toDoubleRest mn mx (O : bs) = toDoubleRest mn ((mn + mx) / 2) bs
toDoubleRest mn mx (I : bs) = toDoubleRest ((mn + mx) / 2) mx bs
