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

instance Enum Size where
	toEnum 0 = Short
	toEnum 1 = Tall
	toEnum 2 = Grande
	toEnum 3 = Venti
	toEnum _ = error "bad argument"
	fromEnum Short = 0
	fromEnum Tall = 1
	fromEnum Grande = 2
	fromEnum Venti = 3
	enumFrom x = enumFromTo x maxBound
	enumFromThen x y = enumFromThenTo x y maxBound
