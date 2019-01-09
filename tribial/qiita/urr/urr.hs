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

data NP = N | P deriving Show

fromNP :: Num a => NP -> a
fromNP N = - 1
fromNP P = 1

data UrrRange = NInf | Zero | PInf | Exp NP Int | Raw Rational deriving Show

lessThan :: (Ord a, Fractional a) => a -> UrrRange -> Bool
lessThan _ NInf = False
lessThan x Zero = x < 0
lessThan _ PInf = True
lessThan x (Exp np n) = x < fromNP np * 2 ^^ n
lessThan x (Raw r) = x < fromRational r

getRational :: UrrRange -> Rational
getRational Zero = 0
getRational (Exp np n) = fromNP np * 2 ^^ n
getRational (Raw r) = r
getRational _ = error "Oops!"

urrSplitter :: UrrRange -> UrrRange -> UrrRange
urrSplitter NInf PInf = Zero
urrSplitter NInf Zero = Exp N 0
urrSplitter Zero PInf = Exp P 0
urrSplitter NInf (Exp N 0) = Exp N 1
urrSplitter (Exp N 0) Zero = Exp N (- 1)
urrSplitter Zero (Exp P 0) = Exp P (- 1)
urrSplitter (Exp P 0) PInf = Exp P 1
urrSplitter NInf (Exp N mx) = Exp N (2 * mx)
urrSplitter (Exp N mn) Zero = Exp N (2 * mn)
urrSplitter Zero (Exp P mx) = Exp P (2 * mx)
urrSplitter (Exp P mn) PInf = Exp P (2 * mn)
urrSplitter (Exp np mn) (Exp _np mx)
	| mn + 1 < mx = Exp np ((mn + mx) `div` 2)
urrSplitter mn mx = Raw $ (getRational mn + getRational mx) / 2

fromUrr :: Fractional a => [Bit] -> a
fromUrr = toValue urrSplitter (fromRational . getRational) NInf PInf
	. heading flipBit

toUrr :: (Ord a, Fractional a) => Word -> a -> [Bit]
toUrr ln = heading flipBit
	. fromValue (Just ln) (\_ _ -> False) urrSplitter lessThan NInf PInf

flipBit :: Bit -> Bit
flipBit O = I
flipBit I = O

heading :: (a -> a) -> [a] -> [a]
heading f (x : xs) = f x : xs
heading _ _ = error "Oops!"
