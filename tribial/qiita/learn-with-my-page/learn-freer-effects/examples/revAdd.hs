{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

revAdd :: forall a . [a] -> [a] -> [a]
revAdd xs ys = rxs ++ ys
	where
	rxs :: [a]
	rxs = reverse xs

revAddAnnot = (\xs ys -> let rxs :: [a]; rxs = reverse xs in rxs ++ ys)
	:: forall a . [a] -> [a] -> [a]

revAddPat :: [a] -> [a] -> [a]
revAddPat (xs :: [a]) ys = rxs ++ ys
	where
	rxs :: [a]
	rxs = reverse xs
