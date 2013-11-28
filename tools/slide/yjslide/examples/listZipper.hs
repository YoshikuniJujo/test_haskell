type ListZipper a = ([a], [a])

goForward, goBack :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)
goBack (xs, b : bs) = (b : xs, bs)

replaceBy :: a -> ListZipper a -> ListZipper a
replaceBy x (_ : xs, bs) = (x : xs, bs)

fromList :: [a] -> ListZipper a
fromList xs = (xs, [])

toList :: ListZipper a -> [a]
toList (xs, bs) = reverse bs ++ xs

(-:) :: a -> (a -> b) -> b
x -: f = f x

test = toList $ fromList [1, 2, 3, 4, 5, 6, 7, 8, 9]
	-: goForward
	-: goForward
	-: goForward
	-: replaceBy 99
	-: goForward
	-: goForward
	-: goForward
	-: replaceBy 222
	-: goBack
	-: goBack
	-: replaceBy 123
