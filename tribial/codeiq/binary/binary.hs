import Control.Applicative
import Control.Arrow
import Data.List
import Data.Bits

main :: IO ()
main = interact $ (++ "\n") . show . length
	. filter ((==) <$> id <*> decimal . reverse . binary)
	. uncurry enumFromTo . (read *** read . tail) . span (/= ',')

binary :: Int -> [Bool]
binary = unfoldr $ \n -> case n of
	0 -> Nothing
	_ -> Just (odd n, n `shiftR` 1)

decimal :: [Bool] -> Int
decimal = foldr ((. (`shiftL` 1)) . (.|.) . fromEnum) 0
