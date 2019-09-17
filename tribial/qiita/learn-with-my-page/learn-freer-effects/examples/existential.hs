{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data T = forall a . MkT [a]

k :: T -> T
k (MkT [t :: a]) = MkT t3
	where
	t3 :: [a]
	t3 = [t, t, t]
k _ = undefined
