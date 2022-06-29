{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeMatch where

class PairInt a b where add :: a -> b -> Int

data Foo s = Foo Int deriving Show

data Bar s = Bar Int deriving Show

instance PairInt (Foo s) (Bar s) where
	Foo f `add` Bar b = f - b

instance {-# OVERLAPPABLE #-} PairInt (Foo s) (Bar t) where
	Foo f `add` Bar b = f + b

sampleFun :: Foo s -> Bar s -> Int
sampleFun = add

-- ^
-- >>> sampleFun (Foo 8) (Bar 9)
-- -1
