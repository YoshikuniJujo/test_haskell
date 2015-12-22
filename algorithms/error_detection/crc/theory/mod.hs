import Data.Bits
import Data.Bool

-- modulo :: [Bool] -> [Bool] -> [Bool]
-- modulo 

sub :: [Bool] -> [Bool] -> Maybe [Bool]
as `sub` bs = 

decimal :: [Bool] -> Integer
decimal = dec 0
	where
	dec s (b : bs) = dec (bool 0 1 b .|. (s `shiftL` 1)) bs
	dec s _ = s

fromDecimal :: Integer -> [Bool]
fromDecimal = fdc []
	where
	fdc s n | n < 1 = s
	fdc s n = fdc (odd n : s) (n `shiftR` 1)
