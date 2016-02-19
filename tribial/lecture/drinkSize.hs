data Size = Short | Tall | Grande | Venti

instance Eq Size where
	Short == Short = True
	Tall == Tall = True
	Grande == Grande = True
	Venti == Venti = True
	_ == _ = False

instance Ord Size where
	Short <= _ = True
	_ <= Short = False
	Tall <= _ = True
	_ <= Tall = False
	Grande <= _ = True
	_ <= Grande = False
	_ <= _ = True

instance Show Size where
	show Short = "Short"
	show Tall = "Tall"
	show Grande = "Grande"
	show Venti = "Venti"

instance Bounded Size where
	minBound = Short
	maxBound = Venti
