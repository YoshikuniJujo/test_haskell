import Prelude hiding (id, (.))

import Control.Category
import Control.Applicative

(&&&) :: Applicative f => f a -> f b -> f (a, b)
(&&&) = liftA2 (,)

arr :: (Category arr, Functor (arr a)) => (a -> b) -> arr a b
arr f = fmap f id

(***) :: (Category arr, Applicative (arr (a, b))) =>
	arr a x -> arr b y -> arr (a, b) (x, y)
f *** g = (arr fst >>> f) &&& (arr snd >>> g)

first :: (Category arr, Applicative (arr (a, b))) => arr a x -> arr (a, b) (x, b)
first f = f *** id

data StaticParser s = SP Bool [s]
newtype DynamicParser s a b = DP ((a, [s]) -> (b, [s]))
data Parser s a b = P (StaticParser s) (DynamicParser s a b)

instance (Bounded s, Enum s) => Category (Parser s) where
	id = P (SP True [minBound .. maxBound]) (DP id)
	P (SP e1 a1) (DP f1) . P (SP e2 a2) (DP f2) = P
		(SP (e1 && e2) (a2 ++ if e2 then a1 else []))
		(DP $ f1 . f2)

instance Functor (Parser s a) where
	fmap f (P sp (DP d)) = P sp $ DP $ first f . d

firstP (P sp (DP f)) = P sp $ DP $ \((x, y), rest) ->
	let (x', rest') = f (x, rest) in ((x', y), rest')
andAndP :: (Bounded s, Enum s) => Parser s a b -> Parser s a c -> Parser s a (b, c)
p1 `andAndP` p2 = firstP p1 . fmap swap id . firstP p2 . fmap swap id . fmap dup id

dup x = (x, x)
swap (x, y) = (y, x)

instance (Bounded s, Enum s) => Applicative (Parser s a) where
	pure x = P (SP True [minBound .. maxBound]) (DP $ first $ const x)
	f <*> x = fmap (uncurry ($)) id . (f `andAndP` x)
