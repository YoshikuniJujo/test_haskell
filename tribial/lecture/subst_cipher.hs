table :: [(Char, Char)]
table = zip ['a' .. 'z'] (['h' .. 'z'] ++ ['a' .. 'g'])

crypt :: String -> Maybe String
crypt = mapM (`lookup` table)

decrypt :: String -> Maybe String
decrypt = mapM (`lookup` map (uncurry $ flip (,)) table)

myMapM :: Applicative m => (a -> m b) -> [a] -> m [b]
myMapM f (x : xs) = x `op` myMapM f xs -- (:) <$> f x <*> myMapM f xs
	where
	op x y = (:) <$> f x <*> y
myMapM f _ = pure []

myMapM' :: (Applicative m, MyTraversable t) => (a -> m b) -> t a -> m (t b)
myMapM' f = foldr op (pure tempty)
	where
	op x y = tcons <$> f x <*> y
{-
myMapM' f (x : xs) = x `op` myMapM' f xs -- (:) <$> f x <*> myMapM f xs
	where
	op x y = (:) <$> f x <*> y
myMapM' f _ = pure []
-}

instance MyTraversable [] where
	tempty = []
	tcons = (:)

class Foldable t => MyTraversable t where
	tempty :: t a
	tcons :: a -> t a -> t a

crypt_ :: String -> Maybe String
crypt_ (c : cs) = (:) <$> lookup c table <*> crypt_ cs
crypt_ _ = Just ""

decrypt_ :: String -> Maybe String
decrypt_ (c : cs) = (:) <$> lookup c (map (uncurry $ flip (,)) table) <*> decrypt_ cs
decrypt_ _ = Just ""
