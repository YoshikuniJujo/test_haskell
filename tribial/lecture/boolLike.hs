class BoolLike a where
	toBool :: a -> Bool

instance BoolLike Bool where
	toBool = id

instance BoolLike Integer where
	toBool 0 = False
	toBool _ = True

instance BoolLike Char where
	toBool '\0' = False
	toBool '0' = False
	toBool _ = True

instance BoolLike (Maybe a) where
	toBool Nothing = False
	toBool _ = True

instance BoolLike a => BoolLike [a] where
	toBool [] = False
	toBool [x] = toBool x
	toBool _ = True

iff :: BoolLike b => b -> a -> a -> a
iff b x y = if toBool b then x else y
