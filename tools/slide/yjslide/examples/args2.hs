{-# LANGUAGE GADTs #-}

args :: Some a => a
args = f "args:"

class Args a where
	num :: a -> Int
	num = const 0

instance Args Int

instance Args a => Args (b -> a) where
	num f = 1 + num (f undefined)

class Some a where
	f :: String -> a

instance (Char ~ a) => Some [a] where
	f = id

instance (Show b, Some a) => Some (b -> a) where
	f x y = f $ x ++ " " ++ show y
