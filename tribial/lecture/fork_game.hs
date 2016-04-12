
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

yours :: (Item -> b -> b) -> b -> Bagage -> b
yours op v (i : is) = i `op` yours op v is
yours op v _ = v

stageItems :: Int -> Stage -> Int
stageItems n (Fork l r) = stageItems (stageItems n r) l
stageItems n _ = 1 + n

stagePoint :: Int -> Stage -> Int
stagePoint p (Fork l r) = stagePoint (stagePoint p r) l
stagePoint p (Goal i) = point i + p

bestItem :: Item -> Stage -> Item
bestItem b (Fork l r) = bestItem (bestItem b r) l
bestItem b (Goal i) = i `max` b

worstItem :: Item -> Stage -> Item
worstItem w (Fork l r) = worstItem (worstItem w r) l
worstItem w (Goal i) = i `min` w

stages :: (Item -> b -> b) -> b -> Stage -> b
stages op v (Fork l r) = stages op (stages op v r) l
stages op v (Goal i) = i `op` v

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

tfoldr :: (a -> b -> b) -> b -> BinTree a -> b
tfoldr op v (Node l r) = tfoldr op (tfoldr op v r) l
tfoldr op v (Leaf x) = x `op` v

type Stage' = BinTree Item

stages' :: (Item -> b -> b) -> b -> Stage' -> b
stages' = tfoldr
