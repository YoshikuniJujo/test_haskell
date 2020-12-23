module TribialTools where

mkTitle :: String -> String
mkTitle ttl =
	replicate 40 '*' ++ "\n*" ++ replicate (19 - s2) ' ' ++ ttl ++ replicate (19 - s1) ' ' ++ "*\n" ++ replicate 40 '*'
	where
	s = length ttl
	s1 = s `div` 2
	s2 = s - s1
	
