data WBT a = Empty | Bin {
	_weight :: Int,
	value :: a,
	left :: WBT a,
	right :: WBT a }

instance Show a => Show (WBT a) where
	show = showTree [] []

bin :: a -> WBT a -> WBT a -> WBT a
bin x t1 t2 = Bin (weight t1 + weight t2 + 1) x t1 t2

weight :: WBT a -> Int
weight (Bin s _ _ _) = s
weight _ = 0

delta, ratio :: Int
delta = 3
ratio = 2

singleton :: a -> WBT a
singleton x = bin x Empty Empty

rotateL, rotateR :: WBT a -> WBT a
rotateL (Bin _ x lx (Bin _ y ly ry)) = bin y (bin x lx ly) ry
rotateL _ = error "rotateL: can't do left rotation"
rotateR (Bin _ x (Bin _ y ly ry) rx) = bin y ly (bin x ry rx)
rotateR _ = error "rotateR: can't do right rotation"

balance :: WBT a -> WBT a
balance Empty = Empty
balance t@(Bin _ x l r)
	| weight l + weight r <= 1 = t
	| weight r > delta * weight l = if weight (left r) >= ratio * weight (right r)
		then rotateL $ bin x l (rotateR r)
		else rotateL t
	| weight l > delta * weight r = if weight (right l) >= ratio * weight (left l)
		then rotateR $ bin x (rotateL l) r
		else rotateR t
	| otherwise = t

insert :: Ord a => a -> WBT a -> WBT a
insert x Empty = singleton x
insert x t@(Bin _ x0 l r)
	| x < x0 = balance $ bin x0 (insert x l) r
	| x > x0 = balance $ bin x0 l (insert x r)
	| otherwise = t

delete :: Ord a => a -> WBT a -> WBT a
delete _ Empty = Empty
delete x (Bin _ x0 l r)
	| x < x0 = balance $ bin x0 (delete x l) r
	| x > x0 = balance $ bin x0 l (delete x r)
	| otherwise = glue l r

glue :: WBT a -> WBT a -> WBT a
glue Empty r = r
glue l Empty = l
glue l r
	| weight l > weight r =
		let (m, l') = deleteFindMax l in balance $ bin m l' r
	| otherwise =
		let (m, r') = deleteFindMin r in balance $ bin m l r'

deleteFindMin :: WBT a -> (a, WBT a)
deleteFindMin (Bin _ x Empty r) = (x, r)
deleteFindMin (Bin _ x l r) =
	let (xm, l') = deleteFindMin l in (xm, balance $ bin x l' r)
deleteFindMin Empty = error "deleteFindMin: can't return the minimal element"

deleteFindMax :: WBT a -> (a, WBT a)
deleteFindMax (Bin _ x l Empty) = (x, l)
deleteFindMax (Bin _ x l r) =
	let (xm, r') = deleteFindMax r in (xm, balance $ bin x l r')
deleteFindMax Empty = error "deleteFindMax: can't return the maximal element"

fromList :: Ord a => [a] -> WBT a
fromList = foldl (flip insert) Empty

showTree :: Show a => [String] -> [String] -> WBT a -> String
showTree lbars _ Empty = showBars lbars ++ "|\n"
showTree lbars _ (Bin _ x Empty Empty) =
	showBars lbars ++ show x ++ "\n"
showTree lbars rbars (Bin _ x l r) =
	showTree (withBar rbars) (withEmpty rbars) r ++
--	concat (reverse rbars) ++ "|\n" ++
	showBars lbars ++ show x ++ "\n" ++
--	concat (reverse lbars) ++ "|\n" ++
	showTree (withEmpty lbars) (withBar lbars) l

showBars :: [String] -> String
showBars [] = ""
showBars bars = concat (reverse (tail bars)) ++ "+--"

withEmpty, withBar :: [String] -> [String]
withEmpty bars = "   " : bars
withBar bars = "|  " : bars

printTree :: Show a => WBT a -> IO ()
printTree = putStr . showTree [] []

testTree :: WBT Int
testTree = fromList [0 .. 15]
