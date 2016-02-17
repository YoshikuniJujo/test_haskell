import Data.List
import qualified Data.Set as S

class IntStorage s where
	empty :: s
	store :: Int -> s -> s
	derive :: s -> Maybe (Int, s)

stored :: IntStorage s => s
stored = let
	s1 = store 8 empty
	s2 = store 5 s1
	s3 = store 2 s2
	s4 = store 10 s3
	s5 = store 3 s4 in
	s5

derive3 :: IntStorage s => s -> Maybe [Int]
derive3 s = case derive s of
	Just (l, s1) -> case derive s1 of
		Just (m, s2) -> case derive s2 of
			Just (n, s3) -> Just [l, m, n]
			_ -> Nothing
		_ -> Nothing
	_ -> Nothing

newtype ListInt = LI [Int] deriving Show

instance IntStorage ListInt where
	empty = LI []
	store n (LI s) = LI $ n : s
	derive (LI (n : s)) = Just (n, LI s)
	derive _ = Nothing

newtype SetInt = SI (S.Set Int) deriving Show

instance IntStorage SetInt where
	empty = SI S.empty
	store n (SI s) = SI $ S.insert n s
	derive (SI s)
		| S.null s = Nothing
		| otherwise = Just (n, SI s')
		where (n, s') = S.deleteFindMin s

data QueueInt = QI [Int] [Int] deriving Show

instance IntStorage QueueInt where
	empty = QI [] []
	store n (QI f r) = QI f (n : r)
	derive (QI (n : f) r) = Just (n, QI f r)
	derive (QI _ r@(_ : _)) = derive $ QI (reverse r) []
	derive _ = Nothing

data OddEven = OE [Int] [Int] deriving Show

instance IntStorage OddEven where
	empty = OE [] []
	store n (OE os es)
		| odd n = OE (n : os) es
		| otherwise = OE os (n : es)
	derive (OE (o : os) es) = Just (o, OE os es)
	derive (OE _ (e : es)) = Just (e, OE [] es)
	derive _ = Nothing

toList :: IntStorage s => s -> [Int]
toList = unfoldr derive

fromList :: IntStorage s => [Int] -> s
fromList = foldr store empty

move :: (IntStorage s, IntStorage t) => s -> t
move s = case derive s of
	Just (x, s') -> x `store` move s'
	_ -> empty
-- move s = maybe empty (uncurry ((. move) . store)) $ derive s
