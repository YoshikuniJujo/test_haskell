import Control.Applicative

takeTo, takeTo', takeTo'' :: (a -> Bool) -> [a] -> [a]
takeTo _ [] = []
takeTo p (x  : xs)
	| p x = [x]
	| otherwise = x : takeTo p xs

takeTo' p = foldr (\x -> (x :) . if p x then const [] else id) []

takeTo'' = (`foldr` []) . ((.) <$> (:) <*>) . ((([id, const []] !!) . fromEnum) .)
