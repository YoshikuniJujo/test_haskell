{-# LANGUAGE RankNTypes, KindSignatures, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Test.TFoldable where

infixr 5 :::

data TList :: (* -> * -> *) -> * -> * -> * where
	EmptyList :: TList c x x
	(:::) :: c x y -> TList c y z -> TList c x z

class TFoldable t where
	tfoldr :: (forall x y z . c x y -> d y z -> d x z) ->
		d y' z' -> t c x' y' -> d x' z'
	tfoldl :: (forall x y z . d x y -> c y z -> d x z) ->
		d x' y' -> t c y' z' -> d x' z'

instance TFoldable TList where
	tfoldr _ d EmptyList = d
	tfoldr (-<) d (c ::: cs) = c -< tfoldr (-<) d cs
	tfoldl _ d EmptyList = d
	tfoldl (>-) d (c ::: cs) = tfoldl (>-) (d >- c) cs

treducer :: TFoldable t => (forall x y z . c x y -> d y z -> d x z) -> t c x' y' -> d y' z' -> d x' z'
treducer (-<) = flip $ tfoldr (-<)

treducel :: TFoldable t => (forall x y z . d x y -> c y z -> d x z) -> d x' y' -> t c y' z' -> d x' z'
treducel = tfoldl
