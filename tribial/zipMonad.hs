import Control.Applicative
import Data.Maybe

instance Monad ZipList where
	ZipList xs >>= f = ZipList . zincat $ map (getZipList . f) xs

zincat :: [[a]] -> [a]
zincat ((x : xs) : xss) = x : zincat (mapMaybe safeTail xss)
zincat _ = []

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

fun :: Int -> Int -> ZipList (Int, Int)
fun x y = ZipList $ replicate (x * y) (x, y)

check :: ZipList (Int, Int)
check = do
	x <- ZipList [3, 2, 1]
	y <- ZipList [3, 1, 2]
	z <- fun x y
	return z

check1 :: ZipList (Int, Int)
check1 =
	ZipList [3, 2, 1] >>= \x ->
	ZipList [2, 1, 3] >>= \y ->
	fun x y >>= \z ->
	return z

check2 :: ZipList (Int, Int)
check2 =
	(ZipList [3, 2, 1] >>= \x ->
	ZipList [2, 1, 3] >>= \y ->
	return (x, y)) >>= \(x, y) ->
	fun x y >>= \z ->
	return z

check3 :: ZipList (Int, Int)
check3 =
	ZipList [3, 2, 1] >>= \x ->
	ZipList [2, 1, 3] >>= \y ->
	return (x, y) >>= \(x, y) ->
	fun x y >>= \z ->
	return z
