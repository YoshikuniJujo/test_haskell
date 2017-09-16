{-# LANGUAGE
	ExistentialQuantification, TypeFamilies,
	DataKinds, TypeOperators,
	ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Unsafe.Coerce

type family Elem t (ts :: [*]) :: Bool where
	Elem _ '[] = 'False
	Elem t (t ': ts) = 'True
	Elem t (_ ': ts) = 'False

data OpenUnion = forall x . OpenUnion x

data List (ts :: [*]) = List [OpenUnion]

empty :: List '[]
empty = List []

cons :: x -> List ts -> List (x : ts)
cons v (List us) = List $ OpenUnion v : us

uncons :: List (x : ts) -> (x, List ts)
uncons (List (OpenUnion u : us)) = (unsafeCoerce u, List us)

class Get v l where
	get :: l -> v

instance Get t (List (t : ts)) where
	get (List (OpenUnion u : _)) = unsafeCoerce u

instance {-# OVERLAPPABLE #-} Get t (List ts) => Get t (List (_s : ts)) where
	get l = get (snd $ uncons l :: List ts)
