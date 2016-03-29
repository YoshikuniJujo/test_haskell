import Data.Monoid
import Data.List
import Data.Function

newtype Max a = Max { getMax :: a } deriving Show

instance (Ord a, Bounded a) => Monoid (Max a) where
	mempty = Max minBound
	mappend = (Max .) . max `on` getMax
	mconcat = foldl' mappend mempty

newtype Min a = Min { getMin :: a } deriving Show

instance (Ord a, Bounded a) => Monoid (Min a) where
	mempty = Min maxBound
	mappend = (Min .) . min `on` getMin
	mconcat = foldl' mappend mempty
