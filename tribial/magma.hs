import Data.Coerce
import Data.Function

class Magma m where
	dot :: m -> m -> m

instance Magma () where
	dot = const $ const ()

newtype And = And { getAnd :: Bool } deriving Show

instance Magma And where
	dot = coerce (&&)

newtype Or = Or { getOr :: Bool } deriving Show

instance Magma Or where
	dot = coerce (||)

newtype Xor = Xor { getXor :: Bool } deriving Show

instance Magma Xor where
	dot = coerce ((/=) :: Bool -> Bool -> Bool)

newtype Add a = Add { getAdd :: a } deriving Show

instance Num a => Magma (Add a) where
	dot = (Add .) . (+) `on` getAdd

newtype Mul a = Mul { getMul :: a } deriving Show

instance Num a => Magma (Mul a) where
	dot = (Mul .) . (*) `on` getMul

newtype Sub a = Sub { getSub :: a } deriving Show

instance Num a => Magma (Sub a) where
	dot = (Sub .) . (-) `on` getSub
