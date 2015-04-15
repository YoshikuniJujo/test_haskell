import Data.List

main :: IO ()
main = interact $ toDigits . read

toDigits :: Int -> String
toDigits = unlines . map concat . transpose . reverse . map (makeDigit . (digits !!)) . unfoldr uncons

uncons :: Int -> Maybe (Int, Int)
uncons 0 = Nothing
uncons n = Just (n `mod` 10, n `div` 10)

makeDigit :: [Bool] -> [String]
makeDigit [a, b, c, d, e, f, g] = [
	["   ", " _ "] !! fromEnum a,
	[" |" !! fromEnum b, " _" !! fromEnum c, " |" !! fromEnum d],
	[" |" !! fromEnum e, " _" !! fromEnum f, " |" !! fromEnum g] ]

digits :: [[Bool]]
digits = [zero, one, two, three, four, five, six, seven, eight, nine]

zero, one, two, three, four, five, six, seven, eight, nine :: [Bool]
zero  = [ True,  True, False,  True,  True, False,  True]
one   = [False, False, False,  True, False, False,  True]
two   = [ True, False,  True,  True,  True,  True, False]
three = [ True, False,  True,  True, False,  True,  True]
four  = [False,  True,  True,  True, False, False,  True]
five  = [ True,  True,  True, False, False,  True,  True]
six   = [ True,  True,  True, False,  True,  True,  True]
seven = [ True, False, False,  True, False, False,  True]
eight = [ True,  True,  True,  True,  True,  True,  True]
nine  = [ True,  True,  True,  True, False,  True,  True]
