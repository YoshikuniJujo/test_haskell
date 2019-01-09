{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word

data Bit = O | I deriving Show

fromString :: String -> [Bit]
fromString = map fc
	where fc '0' = O; fc '1' = I; fc _ = error "Oops!"

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
