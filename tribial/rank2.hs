{-# LANGUAGE Rank2Types #-}

import Data.Tree
import Data.Maybe

fun :: (forall a . [a] -> Int) -> [Int] -> String -> Int
fun f ns ss = f ns + f ss

testEmpty :: [a] -> Int
testEmpty [] = 0
testEmpty (_ : _) = 1

fun1 :: (forall a . a -> a) -> Int -> String -> (Int, String)
fun1 f n s = (f n, f s)

foo :: (forall a . Tree a -> Int) -> Tree Int -> Tree Char -> Int
foo f ns ss = f ns + f ss

bar :: (forall a . Tree a -> Int) -> Tree b -> Tree c -> Int
bar f bs cs = f bs + f cs

hoge :: (forall a . [a] -> Maybe a) ->
	Either String [b] -> Either String (Maybe b)
hoge _ (Left em) = Left em
hoge f (Right xs) = Right $ f xs

hoge' :: ([a] -> Maybe a) -> Either String [a] -> Either String (Maybe a)
hoge' _ (Left em) = Left em
hoge' f (Right xs) = Right $ f xs

piyo = hoge' listToMaybe
