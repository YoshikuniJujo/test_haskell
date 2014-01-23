data OI = O | I deriving Show

class State s where
	step :: s -> OI -> s
	start :: s
	accept :: s -> Bool

run :: State s => s -> [OI] -> s
run = foldl step

isAccept :: State s => s -> [OI] -> Bool
isAccept s = accept . run (asTypeOf start s)

data QEven = QEven | QOdd deriving Show

instance State QEven where
	step QEven _ = QOdd
	step QOdd _ = QEven
	start = QEven
	accept QEven = True
	accept _ = False
