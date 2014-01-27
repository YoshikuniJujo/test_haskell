module Lion (
	Cage, Lion, lion, baptize, feed, play
) where

data Lion = Lion (Maybe Name) State deriving Show

type Name = String
data State = Hungry | Normal | Full deriving Show

newtype Cage a = Cage a deriving Show

instance Monad Cage where
	return = Cage
	Cage x >>= f = f x

lion :: Lion
lion = Lion Nothing Hungry

baptize :: Name -> Lion -> Cage Lion
baptize n (Lion Nothing st) = Cage $ Lion (Just n) st
baptize _ l = Cage l

feed :: Lion -> Lion
feed (Lion n Hungry) = Lion n Normal
feed (Lion n Normal) = Lion n Full
feed (Lion n _) = Lion n Full

play :: Lion -> Lion
play (Lion n Full) = Lion n Normal
play (Lion n Normal) = Lion n Hungry
play (Lion n _) = Lion n Hungry

-- Thomas, Daniel, John, Stephen, Margaret, Barbara
