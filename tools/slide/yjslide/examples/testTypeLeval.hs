{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds #-}

import GHC.Exts (Constraint)

data NAry (as :: [*]) a where
	Value :: a -> NAry '[] a
	Arg :: (b -> NAry xs a) -> NAry (b ': xs) a

type family Nyoro (xs :: [*]) (a :: *)
type instance Nyoro '[] a = a
type instance Nyoro (x ': xs) a = x -> Nyoro xs a

fromNAry :: NAry xs a -> Nyoro xs a
fromNAry (Value a) = a
fromNAry (Arg f) = fromNAry . f

data SList (as :: [*]) where
	SNil :: SList '[]
	SCons :: a -> SList xs -> SList (a ': xs)

toNAry' :: SList xs -> Nyoro xs a -> NAry xs a
toNAry' SNil a = Value a
toNAry' (SCons _ ts) f = Arg $ toNAry' ts . f

class SingList k where
	slist :: SList k

instance SingList '[] where
	slist = SNil

instance SingList xs => SingList (x ': xs) where
	slist = SCons undefined slist

toNAry :: forall xs a . SingList xs => (Nyoro xs a) -> NAry xs a
toNAry = toNAry' (slist :: SList xs)

applyReadList :: Readable as => NAry as a -> [String] -> Maybe a
applyReadList (Value a) [] = Just a
applyReadList (Arg f) (x : xs) = applyReadList (f $ read x) xs
applyReadList _ _ = Nothing

-- type family Readable (as :: [*]) :: Constraint

type family All (cxt :: * -> Constraint) (xs :: [*]) :: Constraint
type instance All cxt '[] = ()
type instance All cxt (x ': xs) = (cxt x, All cxt xs)

type Readable as = All Read as
