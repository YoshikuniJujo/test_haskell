{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module UnionList (
	UnionList(UnionListNil), AddValue, MinValue, Nihil, Expand, Collapse, Merge, Project,
	(>+), (>-), expand, collapse, merge, prj, extract) where

import Data.Kind
import Sorted.Internal hiding (Merge)

data UnionList :: Bool -> Sorted Type -> * where
	UnionListNil :: UnionList b 'Nil
	(:.) :: Maybe a -> UnionList b as -> UnionList b (a ':~ as)

class AddValue a (as :: Sorted Type) (as' :: Sorted Type) where
	(>+.) :: a -> UnionList b as -> UnionList b as'

instance AddValue a as (a ':~ as) where
	x >+. xs = Just x :. xs

instance {-# OVERLAPPABLE #-} AddValue a as as' => AddValue a (a' ':~ as) (a' ':~ as') where
	x >+. (y :. xs) = y :. (x >+. xs)

infixr 5 >+

(>+) :: AddValue a as (a :- as) => a -> UnionList b as -> UnionList b (a :- as)
(>+) = (>+.)

class MinValue a (as :: Sorted Type) (as' :: Sorted Type) where
	(>-.) :: Ord a => a -> UnionList b as -> UnionList b as'

instance MinValue a (a ':~ as) (a ':~ as) where
	x >-. (x' :. xs) = Just (maybe x (x `min`) x') :. xs

instance {-# OVERLAPPABLE #-} MinValue a as (a ':~ as) where
	x >-. xs = Just x :. xs

instance {-# OVERLAPPABLE #-} MinValue a as as' => MinValue a (a' ':~ as) (a' ':~ as') where
	x >-. (y :. xs) = y :. (x >-. xs)

infixr 5 >-

(>-) :: (MinValue a as (a :- as), Ord a) => a -> UnionList b as -> UnionList b (a :- as)
(>-) = (>-.)

class Nihil as where nihil :: UnionList b as

instance Nihil 'Nil where nihil = UnionListNil

instance Nihil as => Nihil (a ':~ as) where
	nihil = Nothing :. nihil

class Expand (b :: Bool) (as :: Sorted Type) (as' :: Sorted Type) where
	expand :: UnionList b as -> UnionList b as'

instance Nihil as => Expand 'False 'Nil as where
	expand UnionListNil = nihil

instance Nihil as => Expand 'True (a ':~ 'Nil) (a ':~ as) where
	expand (x :. UnionListNil) = x :. nihil

instance {-# OVERLAPPABLE #-} Expand b as as' => Expand b (a ':~ as) (a ':~ as') where
	expand (x :. xs) = x :. expand xs

instance {-# OVERLAPPABLE #-} Expand b (a ':~ as) as' => Expand b (a ':~ as) (a' ':~ as') where
	expand xs = Nothing :. expand xs

class Project a (as :: Sorted Type) where
	prj :: UnionList b as -> Maybe a

instance Project a 'Nil where
	prj _ = Nothing

instance Project a (a ':~ as) where
	prj (x :. _) = x

instance {-# OVERLAPPABLE #-} Project a as => Project a (a' ':~ as) where
	prj (_ :. xs) = prj xs

extract :: UnionList 'True (Singleton a) -> a
extract u = case prj u of Just x -> x; Nothing -> error "never occur"

class Collapse b' (as :: Sorted Type) (as' :: Sorted Type) where
	collapse :: UnionList b as -> Maybe (UnionList b' as')

instance {-# INCOHERENT #-} Nihil as' => Collapse 'False 'Nil as' where collapse = const $ Just nihil
instance {-# INCOHERENT #-} Collapse 'True 'Nil as' where collapse = const Nothing
instance {-# INCOHERENT #-} Collapse b' as 'Nil where collapse = const $ Just UnionListNil

instance Collapse b' as as' => Collapse b' (a ':~ as) (a ':~ as') where
	collapse (x :. xs) = (x :.) <$> collapse xs

instance {-# OVERLAPPABLE #-} Collapse b' as as' => Collapse b' (a ':~ as) as' where
	collapse (_ :. xs) = collapse xs

class Merge (as :: Sorted Type) (as' :: Sorted Type) (merged :: Sorted Type) where
	merge_ :: UnionList b as -> UnionList b as' -> UnionList b merged

instance Merge 'Nil 'Nil 'Nil where
	merge_ UnionListNil UnionListNil = UnionListNil

instance (Ord a, Merge as as' merged) => Merge (a ':~ as) (a ':~ as') (a ':~ merged) where
	merge_ (x :. xs) (x' :. xs') = (x `min` x') :. merge_ xs xs'

instance {-# OVERLAPPABLE #-} Merge as as' merged => Merge (a ':~ as) as'  (a ':~ merged) where
	merge_ (x :. xs) xs' = x :. merge_ xs xs'

instance {-# OVERLAPPABLE #-} Merge as as' merged => Merge as (a ':~ as') (a ':~ merged) where
	merge_ xs (x :. xs') = x :. merge_ xs xs'

merge :: Merge as as' (as :+: as') => UnionList b as -> UnionList b as' -> UnionList b (as :+: as')
merge = merge_
	
numbered [t| Bool |]
numbered [t| Char |]
numbered [t| Integer |]
numbered [t| Double |]
