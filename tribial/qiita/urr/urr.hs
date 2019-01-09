{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word

data Bit = O | I deriving Show

fromString :: String -> [Bit]
fromString = map fc where fc '0' = O; fc '1' = I; fc _ = error "Oops!"

toString :: [Bit] -> String
toString = map $ \case O -> '0'; I -> '1'

toValue :: (r -> r -> r) -> (r -> a) -> r -> r -> [Bit] -> a
toValue _ gv mn _ [] = gv mn
toValue sp gv mn mx (O : bs) = toValue sp gv mn (sp mn mx) bs
toValue sp gv mn mx (I : bs) = toValue sp gv (sp mn mx) mx bs

fromValue ::
	Maybe Word -> (r -> r -> Bool) -> (r -> r -> r) -> (a -> r -> Bool) ->
	r -> r -> a -> [Bit]
fromValue ln fn _ _ mn mx _ | Just 0 <- ln = [] | fn mn mx = []
fromValue ln fn sp lt mn mx x
	| x `lt` s = O : fromValue (subtract 1 <$> ln) fn sp lt mn s x
	| otherwise = I : fromValue (subtract 1 <$> ln) fn sp lt s mx x
	where s = sp mn mx

toWord8 :: Int -> Int -> [Bit] -> Word8
toWord8 = toValue (\a b -> (a + b) `div` 2) fromIntegral

fromWord8 :: Int -> Int -> Word8 -> [Bit]
fromWord8 = fromValue Nothing
	(\a b -> a + 1 >= b)
	(\a b -> (a + b) `div` 2)
	(\w s -> fromIntegral w < s)

positiveIntegerSplitter :: Maybe Integer -> Maybe Integer -> Maybe Integer
positiveIntegerSplitter (Just mn) Nothing = Just $ mn * 2
positiveIntegerSplitter (Just mn) (Just mx) = Just $ (mn + mx) `div` 2
positiveIntegerSplitter _ _ = error "Oops!"

toPositiveInteger :: [Bit] -> Integer
toPositiveInteger = toValue
	positiveIntegerSplitter
	(\case	Just mn -> mn; _ -> error "Oops!")
	(Just 1) Nothing

fromPositiveInteger :: Integer -> [Bit]
fromPositiveInteger = fromValue Nothing
	(\a b -> case (a, b) of
		(Just mn, Just mx) -> mn + 1 >= mx
		_ -> False)
	positiveIntegerSplitter
	(\n -> maybe True (n <))
	(Just 1) Nothing

integerSplitter :: Maybe Integer -> Maybe Integer -> Maybe Integer
integerSplitter Nothing Nothing = Just 0
integerSplitter Nothing (Just 0) = Just $ - 1
integerSplitter (Just 0) Nothing = Just 1
integerSplitter Nothing (Just mx) = Just $ mx * 2
integerSplitter (Just mn) Nothing = Just $ mn * 2
integerSplitter (Just mn) (Just mx) = Just $ (mn + mx) `div` 2

toWholeInteger :: [Bit] -> Maybe Integer
toWholeInteger = toValue integerSplitter id Nothing Nothing

fromWholeInteger :: Integer -> [Bit]
fromWholeInteger = fromValue Nothing
	(\a b -> case (a, b) of
		(Just mn, Just mx) -> mn + 1 >= mx
		_ -> False)
	integerSplitter
	(\n -> maybe (error "Oops!") (n <))
	Nothing Nothing
