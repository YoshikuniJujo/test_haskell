{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Unsafe.Coerce

data UnionValue = forall x . UnionValue x

hetero :: [UnionValue]
hetero = [
	UnionValue (123 :: Integer), UnionValue True,
	UnionValue (), UnionValue 'c' ]

fromHetero :: [UnionValue] -> (Integer, Bool, (), Char)
fromHetero [UnionValue n, UnionValue b, UnionValue u, UnionValue c] = (
	unsafeCoerce n, unsafeCoerce b, unsafeCoerce u, unsafeCoerce c )

doubleValue :: UnionValue
doubleValue = UnionValue (123 :: Double)

data Union a = forall t . Union (t a)

data State s a where
	Get :: State s s
	Put :: s -> State s ()

data Exc e a where
	Exc :: e -> Exc e a
	deriving Show

data Writer w a where
	Writer :: w -> Writer w ()

effects :: [Union ()]
effects = [
	Union $ Put (123 :: Integer),
	Union $ Exc "hello",
	Union $ Writer "world" ]

fromUnion :: Union a -> t a
fromUnion (Union tx) = unsafeCoerce tx
