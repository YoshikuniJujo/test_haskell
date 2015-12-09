import Control.Arrow

data Fizz = Fizz deriving Show
data Buzz = Buzz deriving Show

main :: IO ()
main = putStr . unlines . take 100 . (`map` [1 ..]) $ uncurry (++)
	. (uncurry (++) . (str *** str) *** str) . (fst &&& number)
	. ((fizz &&& buzz) &&& id)

str :: Show a => Maybe a -> String
str = maybe "" show

number :: ((Maybe Fizz, Maybe Buzz), Int) -> Maybe Int
number ((Nothing, Nothing), n) = Just n
number _ = Nothing

fizz :: Int -> Maybe Fizz
fizz n | n `mod` 3 == 0 = Just Fizz | otherwise = Nothing

buzz :: Int -> Maybe Buzz
buzz n | n `mod` 5 == 0 = Just Buzz | otherwise = Nothing
