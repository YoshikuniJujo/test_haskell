{-# OPTIONS_GHC -fno-warn-tabs #-}

newtype Fixed a = MkFixed Integer deriving (Eq, Ord)

class HasResolution a where
	resolution :: p a -> Integer

instance HasResolution a => Show (Fixed a) where
	show fa@(MkFixed a) =
		show (a `div` resolution fa) ++ "." ++
		show (a `mod` resolution fa)

instance HasResolution a => Num (Fixed a) where
	MkFixed a + MkFixed b = MkFixed $ a + b
	MkFixed a * MkFixed b = MkFixed $ a * b
	negate (MkFixed a) = MkFixed $ negate a
	abs (MkFixed a) = MkFixed $ abs a
	signum (MkFixed a) = fromInteger (signum a)
	fromInteger i = withResolution $ \res -> MkFixed $ i * res

instance HasResolution a => Fractional (Fixed a) where
	fa@(MkFixed a) / (MkFixed b) = MkFixed $ a * resolution fa `div` b
	fromRational r = withResolution $ \res ->
		MkFixed . floor $ r * toRational res

withType :: (p a -> f a) -> f a
withType foo = foo undefined

withResolution :: HasResolution a => (Integer -> f a) -> f a
withResolution foo = withType (foo . resolution)

data E0
instance HasResolution E0 where
	resolution _ = 1
type Uni = Fixed E0

data E1
instance HasResolution E1 where
	resolution _ = 10
type Deci = Fixed E1

data E2
instance HasResolution E2 where
	resolution _ = 100
type Centi = Fixed E2

data E3
instance HasResolution E3 where
	resolution _ = 1000
type Milli = Fixed E3
