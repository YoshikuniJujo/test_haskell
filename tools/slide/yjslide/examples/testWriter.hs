{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.Writer

two :: Writer String Int
two = do
	tell "This is number 2.\n"
	return 2

add :: Int -> Int -> Writer String Int
add x y = do
	tell "Addition done.\n"
	return $ x + y

twoPlusTwo :: Writer String Int
twoPlusTwo = do
	x <- two
	y <- two
	add x y
