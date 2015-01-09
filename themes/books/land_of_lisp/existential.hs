{-# LANGUAGE ExistentialQuantification #-}

data Look = forall a . Show a => Look a

instance Show Look where
	show (Look x) = show x

hetero :: [Look]
hetero = [Look 8, Look 8.5, Look "hoge", Look [Look 8, Look "hoge"]]
