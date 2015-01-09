data MyT = MyT Int Bool Char

instance Show MyT where
	show (MyT i b c) = "MyT " ++
		show i ++ " " ++ show b ++ " " ++ show c
