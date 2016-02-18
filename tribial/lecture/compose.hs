class Composable a where
	compose :: a -> a -> a

newtype Add = Add { getAdd :: Int } deriving Show

instance Composable Add where
	compose (Add x) (Add y) = Add $ x + y

newtype Mul = Mul { getMul :: Int } deriving Show

instance Composable Mul where
	compose (Mul x) (Mul y) = Mul $ x * y
