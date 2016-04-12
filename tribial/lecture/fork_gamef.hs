import Data.Monoid

data Item = Banana | Club | Shield | Armor | Sord | Star
	deriving (Show, Eq, Ord, Bounded)

type Bagage = [Item]

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show
type Stage = BinTree Item

instance Foldable BinTree where
	foldMap f (Node l r) = foldMap f l `mappend` foldMap f r
	foldMap f (Leaf x) = f x

point :: Item -> Int
point Banana = 10
point Club = 20
point Shield = 50
point Armor = 80
point Sord = 100
point Star = 200

yourItems :: Bagage -> Int
yourItems = length

yourPoint :: Bagage -> Int
yourPoint = calcPoint

calcPoint :: Foldable f => f Item -> Int
calcPoint = getSum . foldMap (Sum . point)

doesStarExist :: Stage -> Bool
doesStarExist = (Star `elem`)

stageItems :: Stage -> Int
stageItems = length

stagePoint :: Stage -> Int
stagePoint = calcPoint

bestItem :: Stage -> Item
bestItem = maximum

worstItem :: Stage -> Item
worstItem = minimum

bagage1, bagage2, bagage3 :: Bagage
bagage1 = [Banana, Armor, Star, Sord]
bagage2 = [Club, Armor, Shield, Star, Star]
bagage3 = [Shield, Sord, Star]

stage1, stage2, stage3 :: Stage
stage1 = Node
	(Node (Leaf Club) (Leaf Sord))
	(Node (Leaf Shield) (Leaf Club))

stage2 = Node
	(Node
		(Leaf Banana) 
		(Node
			(Leaf Shield)
			(Leaf Banana)))
	(Node
		(Leaf Star)
		(Leaf Sord))

stage3 = Node
	(Leaf Shield)
	(Node
		(Node
			(Leaf Armor)
			(Leaf Shield))
		(Leaf Shield))
