import Data.Monoid
import Data.Function

newtype Max a = Max { getMax :: a } deriving Show

instance (Ord a, Bounded a) => Monoid (Max a) where
	mempty = Max minBound
--	mappend = (Max .) . max `on` getMax
	mappend (Max x) (Max y) = Max $ max x y
