import Control.Applicative
import Data.Maybe

instance Monad ZipList where
	ZipList xs >>= f
		| sameLength yss = ZipList $ zincat yss
		| otherwise = ZipList []
		where
		yss = map (getZipList . f) xs

zincat :: [[a]] -> [a]
zincat ((x : _) : xss) = x : zincat (takeJusts $ map safeTail xss)
zincat _ = []

sameLength :: [[a]] -> Bool
sameLength = and . (zipWith (==) <$> id <*> tail) . map length 

zincatMap :: (a -> [b]) -> [a] -> [b]
zincatMap = (zincat .) . map

takeJusts :: [Maybe a] -> [a]
takeJusts (Just x : mxs) = x : takeJusts mxs
takeJusts _ = []

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

fun :: Int -> ZipList Int
fun n = ZipList $ replicate n n

check :: ZipList Int
check = do
	x <- ZipList [3, 2, 1]
	y <- (x *) <$> ZipList [2, 1, 3]
	fun y

check1 :: ZipList Int
check1 = ZipList [3, 2, 1] >>= \x -> (x *) <$> ZipList [2, 1, 3] >>= fun

check2 :: ZipList Int
check2 = (ZipList [3, 2, 1] >>= \x -> (x*) <$> ZipList [2, 1, 3]) >>= fun

tuples :: Monad m => m a -> m b -> m (a, b)
tuples m1 m2 = do
	x <- m1
	y <- m2
	return (x, y)

tuplesL :: [a] -> [b] -> [(a, b)]
tuplesL m1 m2 =
	(`concatMap` m1) $ \x ->
	(`concatMap` m2) $ \y ->
	[(x, y)]

tuplesZ :: [a] -> [b] -> [(a, b)]
tuplesZ m1 m2 =
	(`zincatMap` m1) $ \x ->
	(`zincatMap` m2) $ \y ->
	repeat (x, y)

{-
zincat :: [[a]] -> [a]
zincat ((x : xs) : xss) = x : zincat (takeJusts $ map safeTail xss)
zincat _ = []
-}
