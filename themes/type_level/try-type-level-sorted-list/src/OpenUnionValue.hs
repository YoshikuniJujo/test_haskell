{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, KindSignatures, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnionValue (
	UnionValue, Member, Elem(..), inj, prj, extract,
	Convert, convert, intersection, intersection' ) where

import Data.Kind
import Unsafe.Coerce

import Sorted.Internal

newtype P (a :: Type) (as :: Sorted Type) = P { unP :: Word } deriving Show

class Member (a :: Type) (as :: Sorted Type) where elemNo :: P a as
instance Member a (a ':~ as) where
	elemNo = P 0
instance {-# OVERLAPPABLE #-} Member a as => Member a (_a' ':~ as) where
	elemNo = P $ 1 + unP (elemNo :: P a as)

data UnionValue (as :: Sorted Type) = forall a . UnionValue Word a

inj :: forall a as . Member a as => a -> UnionValue as
inj = UnionValue $ unP (elemNo :: P a as)

prj :: forall a as . Member a as => UnionValue as -> Maybe a
prj (UnionValue i x)
	| i == unP (elemNo :: P a as) = Just $ unsafeCoerce x
	| otherwise = Nothing

{-
decomp :: UnionValue (a ':~ as) -> Either (UnionValue as) a
decomp (UnionValue 0 x) = Right $ unsafeCoerce x
decomp (UnionValue i x) = Left $ UnionValue (i - 1) x
-}

extract :: UnionValue (a ':~ 'Nil) -> a
extract (UnionValue _ x) = unsafeCoerce x

foo :: UnionValue as -> UnionValue (a ':~ as)
foo (UnionValue i x) = UnionValue (i + 1) x

class Elem (a :: Type) (as :: Sorted Type) where elemValue :: a -> Maybe a
instance Elem a 'Nil where elemValue = const Nothing
instance Elem a (a ':~ as) where elemValue = Just
instance {-# OVERLAPPABLE #-} Elem a as => Elem a (a' ':~ as) where elemValue x = elemValue @a @as x

class Convert (as :: Sorted Type) (as' :: Sorted Type) where
	convert :: UnionValue as -> Maybe (UnionValue as')
instance {-# INCOHERENT #-} Convert 'Nil as' where convert = const Nothing
instance {-# INCOHERENT #-} Convert as 'Nil where convert = const Nothing
instance Convert as as' => Convert (a ':~ as) (a ':~ as') where
	convert (UnionValue 0 x) = Just $ UnionValue 0 x
	convert (UnionValue i x) = foo <$> convert (UnionValue (i - 1) x :: UnionValue as)
instance {-# OVERLAPPABLE #-} (Convert as (a' ':~ as'), Convert (a ':~ as) as') =>
	Convert (a ':~ as) (a' ':~ as') where
	convert u@(UnionValue 0 _) = foo <$> (convert (u :: UnionValue (a ':~ as)) :: Maybe (UnionValue as'))
	convert (UnionValue i x) = convert (UnionValue (i - 1) x :: UnionValue as)

intersection :: [UnionValue as] -> [UnionValue as] -> [UnionValue as]
intersection as bs = filter (`elemIndexOf` (index <$> bs)) as

intersection' :: [UnionValue (Map f as)] -> [UnionValue as] -> [UnionValue (Map f as)]
intersection' as bs = filter (`elemIndexOf` (index <$> bs)) as

index :: UnionValue as -> Word
index (UnionValue i _) = i

-- hasIndexOf :: UnionValue as -> Word -> Bool
-- hasIndexOf (UnionValue i _) i0 = i == i0

elemIndexOf :: UnionValue as -> [Word] -> Bool
elemIndexOf (UnionValue i _) is = i `elem` is
