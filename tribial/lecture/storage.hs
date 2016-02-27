import qualified Data.Set as S

class Storage s where
	empty :: s a
	store :: Ord a => a -> s a -> s a
	derive :: s a -> Maybe (a, s a)

instance Storage [] where
	empty = []
	store = (:)
	derive (x : xs) = Just (x, xs)
	derive _ = Nothing

instance Storage S.Set where
	empty = S.empty
	store = S.insert
	derive s
		| S.null s = Nothing
		| otherwise = Just $ S.deleteFindMin s

data Queue a = Queue [a] [a] deriving Show

instance Storage Queue where
	empty = Queue [] []
	store x (Queue f r) = Queue f (x : r)
	derive (Queue [] []) = Nothing
	derive (Queue (n : f) r) = Just (n, Queue f r)
	derive (Queue _ r) = derive $ Queue (reverse r) []

stored :: Storage s => s Char
stored = let
	s1 = store 'j' empty
	s2 = store 'u' s1
	s3 = store 'j' s2
	s4 = store 'o' s3 in
	s4

derive2 :: Storage s => s Char -> Maybe [Char]
derive2 s = case derive s of
	Just (x, s') -> case derive s' of
		Just (y, s'') -> Just [x, y]
		_ -> Nothing
	_ -> Nothing
