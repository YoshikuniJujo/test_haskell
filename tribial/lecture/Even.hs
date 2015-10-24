module Even (Even, toEven, fromEven, double, add, sub, mul, neg, div2) where

data Even = Even Integer deriving Show

toEven :: Integer -> Maybe Even
toEven n | even n = Just $ Even n | otherwise = Nothing

fromEven :: Even -> Integer
fromEven (Even n) = n

double :: Integer -> Even
double n = Even $ 2 * n

add :: Even -> Even -> Even
Even n1 `add` Even n2 = Even $ n1 + n2

neg :: Even -> Even
neg (Even n) = Even $ - n

sub :: Even -> Even -> Even
e1 `sub` e2 = e1 `add` neg e2

mul :: Even -> Integer -> Even
Even n1 `mul` n2 = Even $ n1 * n2

div2 :: Even -> Either Integer Even
div2 (Even n)
	| odd n' = Left n'
	| otherwise = Right $ Even n'
	where n' = n `div` 2
