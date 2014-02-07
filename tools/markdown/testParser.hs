{-# LANGUAGE QuasiQuotes, TypeFamilies, PackageImports #-}

import Control.Arrow
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.Char
import System.Environment
import Text.Papillon

type List = [List1]
data List1 = Item String List deriving Show

main :: IO ()
main = do
	fn : _ <- getArgs
	cnt <- readFile fn
	print $ parseList cnt

parseList :: String -> Maybe List
parseList src = case flip runState (0, [-1]) $ runErrorT $ list $ parse src of
	(Right (r, _), _) -> Just r
	_ -> Nothing

checkState :: String -> Maybe (Int, [Int])
checkState src = case flip runState (0, [-1]) $ runErrorT $ list $ parse src of
	(_, s) -> Just s

reset :: State (Int, [Int]) Bool
reset = modify (first $ const 0) >> return True

cntSpace :: State (Int, [Int]) ()
cntSpace = modify $ first (+ 1)

deeper :: State (Int, [Int]) Bool
deeper = do
	(n, n0 : ns) <- get
	if n > n0 then put (n, n : n0 : ns) >> return True else return False

same :: State (Int, [Int]) Bool
same = do
	(n, n0 : _) <- get
	return $ n == n0

shallow :: State (Int, [Int]) Bool
shallow = do
	(n, n0 : ns) <- get
	if n < n0 then put (n, ns) >> return True else return False

[papillon|

monad: State (Int, [Int])

list :: List
	= _:countSpace _:dmmy[deeper] l:list1 ls:list1'* _:shllw
						{ return $ l : ls }

list1' :: List1
	= _:countSpace _:dmmy[same] l:list1	{ return l }

list1 :: List1 = '*' ' ' l:line '\n' ls:list?
			{ return $ Item l $ fromMaybe [] ls }

line :: String
	= l:<isLower>+				{ return l }

countSpace :: ()
	= _:dmmy[reset] _:(' ' { cntSpace })*

shllw :: ()
	= _:dmmy[shallow]
	/ !_

dmmy :: () =

|]
