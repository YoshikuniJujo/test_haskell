module Lion (Lion, Caged, lion, feed, play) where

data Lion = Lion Name State deriving Show

type Name = String
data State = Hungry | Normal | Full deriving Show

newtype Caged a = Caged a deriving Show

instance Monad Caged where
	return = Caged
	Caged x >>= f = f x

lion :: Name -> Caged Lion
lion n = return $ Lion n Hungry

feed, play :: Lion -> Lion

feed (Lion n Hungry) = Lion n Normal
feed (Lion n Normal) = Lion n Full
feed (Lion n _) = Lion n Full

play (Lion n Full) = Lion n Normal
play (Lion n Normal) = Lion n Hungry
play (Lion n _) = Lion n Hungry
