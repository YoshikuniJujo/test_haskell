
data Item = Banana | Club | Shield | Armor | Sord | Star
	deriving (Show, Eq, Ord)

type Bagage = [Item]

data Stage = Fork Stage Stage | Goal Item deriving Show

point :: Item -> Int
point Banana = 10
point Club = 20
point Shield = 50
point Armor = 80
point Sord = 100
point Star = 200

bagage1, bagage2, bagage3 :: Bagage
bagage1 = [Banana, Armor, Star, Sord]
bagage2 = [Club, Armor, Shield, Star, Star]
bagage3 = [Shield, Sord, Star]

stage1, stage2, stage3 :: Stage
stage1 = Fork
	(Fork (Goal Club) (Goal Sord))
	(Fork (Goal Shield) (Goal Club))

stage2 = Fork
	(Fork
		(Goal Banana) 
		(Fork
			(Goal Shield)
			(Goal Banana)))
	(Fork
		(Goal Star)
		(Goal Sord))

stage3 = Fork
	(Goal Shield)
	(Fork
		(Fork
			(Goal Armor)
			(Goal Shield))
		(Goal Shield))

yourItems :: Bagage -> Int
yourItems (_ : is) = 1 + yourItems is
yourItems _ = 0

yourPoint :: Bagage -> Int
yourPoint (i : is) = point i + yourPoint is
yourPoint _ = 100

yours :: (Item -> b -> b) -> Bagage -> b -> b
yours op (i : is) = op i . yours op is
yours _ _ = id

doesStarExist :: Stage -> Bool -> Bool
doesStarExist (Fork l r) = doesStarExist l . doesStarExist r
doesStarExist (Goal Star) = const True
doesStarExist _ = id

stageItems :: Stage -> Int -> Int
stageItems (Fork l r) = stageItems l . stageItems r
stageItems _ = (1 +)

stagePoint :: Stage -> Int -> Int
stagePoint (Fork l r) = stagePoint l . stagePoint r
stagePoint (Goal i) = (point i +)

bestItem :: Stage -> Item -> Item
bestItem (Fork l r) = bestItem l . bestItem r
bestItem (Goal i) = (i `max`)

worstItem :: Stage -> Item -> Item
worstItem (Fork l r) = worstItem l . worstItem r
worstItem (Goal i) = (i `min`)

stages :: (Item -> b -> b) -> Stage -> b -> b
stages op (Fork l r) = stages op l . stages op r
stages op (Goal i) = (i `op`)

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

tffoldr :: (a -> b -> b) -> BinTree a -> b -> b
tffoldr op (Node l r) = tffoldr op l . tffoldr op r
tffoldr op (Leaf x) = (x `op`)

type Stage' = BinTree Item

stages' :: (Item -> b -> b) -> Stage' -> b -> b
stages' = tffoldr
