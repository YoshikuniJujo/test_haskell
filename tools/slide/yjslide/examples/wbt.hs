-- data WBT a = Empty | Bin Int a (WBT a) (WBT a) -- deriving Show
data WBT a = Empty | Bin {
	weight :: Int,
	value :: a,
	left :: WBT a,
	right :: WBT a }

instance Show a => Show (WBT a) where
	show = showTree [] []

delta, ratio :: Int
delta = 3
ratio = 2

bin :: a -> WBT a -> WBT a -> WBT a
bin x t1@(Bin s1 _ _ _) t2@(Bin s2 _ _ _) = Bin (s1 + s2 + 1) x t1 t2
bin x t1@(Bin s1 _ _ _) Empty = Bin (s1 + 1) x t1 Empty
bin x Empty t2@(Bin s2 _ _ _) = Bin (s2 + 1) x Empty t2
bin x Empty Empty = Bin 1 x Empty Empty

singleton :: a -> WBT a
singleton x = bin x Empty Empty

size :: WBT a -> Int
size (Bin s _ _ _) = s
size _ = 0

{-
value :: WBT a -> a
value (Bin _ x _ _) = x
value _ = error "value: can't get value"

left, right :: WBT a -> WBT a
left (Bin _ _ l _) = l
left _ = error "left: can't get left tree"
right (Bin _ _ _ r) = r
right _ = error "right: can't get right tree"
-}

rotateL, rotateR :: WBT a -> WBT a
rotateL (Bin _ x lx (Bin _ y ly ry)) = bin y (bin x lx ly) ry
rotateL _ = error "rotateL: can't do left rotation"
rotateR (Bin _ x (Bin _ y ly ry) rx) = bin y ly (bin x ry rx)
rotateR _ = error "rotateR: can't do right rotation"

balance :: WBT a -> WBT a
balance Empty = Empty
balance t@(Bin _ x l r)
	| size l + size r <= 1 = t
	| size r > delta * size l = if size (left r) >= ratio * size (right r)
		then rotateL $ bin x l (rotateR r)
		else rotateL t
	| size l > delta * size r = if size (right l) >= ratio * size (left l)
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
	| size l > size r =
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
