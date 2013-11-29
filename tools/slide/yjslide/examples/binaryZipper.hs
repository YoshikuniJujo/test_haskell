import Prelude hiding (Either(..))

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

data Dir = Left | Right deriving Show

data Log a = Log Dir a (Tree a) deriving Show
-- type Log a = (Dir, a, Tree a)

type Zipper a = (Tree a, [Log a])

goLeft, goRight, goUp :: Zipper a -> Zipper a
goLeft (Node x l r, logs) = (l, Log Left x r : logs)
goRight (Node x l r, logs) = (r, Log Right x l : logs)
-- goLeft (Node x l r, logs) = (l, (Left, x, r) : logs)
-- goRight (Node x l r, logs) = (r, (Right, x, l) : logs)

goUp (t, Log Left x r : logs) = (Node x t r, logs)
goUp (t, Log Right x l : logs) = (Node x l t, logs)
-- goUp (t, (Left, x, r) : logs) = (Node x t r, logs)
-- goUp (t, (Right, x, l) : logs) = (Node x l t, logs)

topMost :: Zipper a -> Zipper a
topMost z@(_, []) = z
topMost z = topMost $ goUp z

fromTree :: Tree a -> Zipper a
fromTree t = (t, [])

toTree :: Zipper a -> Tree a
toTree z = let (t, []) = topMost z in t

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, logs) = (Node (f x) l r, logs)
modify _ (Empty, logs) = (Empty, logs)

put :: a -> Zipper a -> Zipper a
put x (Node _ l r, logs) = (Node x l r, logs)
put x (Empty, logs) = (Node x Empty Empty, logs)

(-:) :: a -> (a -> b) -> b
x -: f = f x

freeTree :: Tree Char
freeTree = toTree $ fromTree Empty
	-: put 'P'
	-: goLeft -: put 'O'
	-: goLeft -: put 'L'
	-: goLeft -: put 'N'
	-: goUp -: goRight -: put 'T'
	-: goUp -: goUp -: goRight -: put 'Y'
	-: goLeft -: put 'S'
	-: goUp -: goRight -: put 'A'
	-: goUp -: goUp -: goUp -: goRight -: put 'L'
	-: goLeft -: put 'W'
	-: goLeft -: put 'C'
	-: goUp -: goRight -: put 'R'
	-: goUp -: goUp -: goRight -: put 'A'
	-: goLeft -: put 'A'
	-: goUp -: goRight -: put 'C'

newTree :: Tree Char
newTree = toTree $ fromTree freeTree
	-: goRight -: goLeft -: modify succ

testTree :: Tree String
testTree = toTree $ fromTree Empty
	-: put "life"
	-: goLeft -: put "plant"
	-: goLeft -: put "tree"
	-: goUp -: goRight -: put "grass"
	-: goUp -: goUp -: goRight -: put "animal"
	-: goLeft -: put "human"
	-: goUp -: goRight -: put "dog"

notEmpty :: Tree a -> Bool
notEmpty Empty = False
notEmpty _ = True

showTree :: Show a => Int -> [Int] -> Tree a -> String
showTree _ _ Empty = ""
showTree i is (Node x l r) =
	stHead i (reverse is) ++ show x ++ "\n" ++
	(if notEmpty l
		then showTree (i + 1) (i : is) l
		else "") ++
	(if notEmpty r
		then showTree (i + 1) is r
		else "")

stHead :: Int -> [Int] -> String
stHead 0 _ = ""
stHead i is = changes 0 (i - 1) is "    " " |  " ++ " +- "

changes :: Int -> Int -> [Int] -> String -> String -> String
changes k l [] b _ = concat $ replicate (l - k) b
changes k l ia@(i : is) b a
	| k >= l = ""
	| k == i = a ++ changes (k + 1) l is b a
	| otherwise = b ++ changes (k + 1) l ia b a

printTree :: Show a => Tree a -> IO ()
printTree = putStr . showTree 0 []

empty = not . notEmpty

simpleShowTree :: Show a => Int -> Tree a -> String
simpleShowTree _ Empty = ""
simpleShowTree i (Node x l r) =
	replicate (i * 4) ' ' ++ show x ++ "\n" ++
	(if empty l then "" else simpleShowTree (i + 1) l) ++
	(if empty r then "" else simpleShowTree (i + 1) r)

simplePrintTree :: Show a => Tree a -> IO ()
simplePrintTree = putStr . simpleShowTree 0
