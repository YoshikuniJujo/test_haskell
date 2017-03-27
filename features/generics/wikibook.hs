import Data.Bits
import Data.List
import Data.Bool

data Bit = O | I deriving (Show, Enum)

class Serialize a where
	put :: a -> [Bit]

instance Serialize Int where
	put i = serializeInt i

instance Serialize a => Serialize [a] where
	put [] = []
	put (h : t) = put h ++ put t

serializeInt :: Int -> [Bit]
serializeInt =
	(\b -> ((++) <$> id <*> (`replicate` b) . (64 -) . length) . serializeBits)
		<$> (bool O I . (< 0))
		<*> id

serializeBits :: (Num n, Bits n) => n -> [Bit]
serializeBits = unfoldr maybePop

maybePop :: (Num n, Bits n) => n -> Maybe (Bit, n)
maybePop = bool Nothing
	<$> Just . pop
	<*> ((&&) <$> (/= zeroBits) <*> (/= zeroBits) . complement)

pop :: (Num n, Bits n) => n -> (Bit, n)
pop = (,) <$> toEnum . fromEnum . (`testBit` 0) <*> (`shiftR` 1)
