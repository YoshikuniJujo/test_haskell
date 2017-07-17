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
