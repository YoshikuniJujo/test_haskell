{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.UnionList (
	UnionList(UnionListNil), AddValue, MinValue, Nihil, Expand, Collapsable, Mergeable, Project,
	(>+), (>+.), singleton, (>-), expand, collapse, merge, merge_, mergeMaybes, prj, extract) where

import GHC.Stack
import Data.Kind
import Data.Type.Set.Internal hiding (Merge)

data UnionList :: Bool -> Set Type -> * where
	UnionListNil :: UnionList b 'Nil
	(:.) :: Maybe a -> UnionList b as -> UnionList b (a ':~ as)

class AddValue a (as :: Set Type) (as' :: Set Type) where
	(>+.) :: a -> UnionList b as -> UnionList b as'

instance AddValue a as (a ':~ as) where
	x >+. xs = Just x :. xs

instance {-# OVERLAPPABLE #-} AddValue a as as' => AddValue a (a' ':~ as) (a' ':~ as') where
	x >+. (y :. xs) = y :. (x >+. xs)

infixr 5 >+

(>+) :: AddValue a as (a :- as) => a -> UnionList b as -> UnionList b (a :- as)
(>+) = (>+.)

singleton :: a -> UnionList b (Singleton a)
singleton = (>+ UnionListNil)

class MinValue a (as :: Set Type) (as' :: Set Type) where
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
instance Nihil as => Nihil (a ':~ as) where nihil = Nothing :. nihil

class Expand (b :: Bool) (as :: Set Type) (as' :: Set Type) where
	expand :: UnionList b as -> UnionList b as'

instance Nihil as => Expand 'False 'Nil as where
	expand UnionListNil = nihil

instance Nihil as => Expand 'True (a ':~ 'Nil) (a ':~ as) where
	expand (x :. UnionListNil) = x :. nihil

instance {-# OVERLAPPABLE #-} Expand b as as' => Expand b (a ':~ as) (a ':~ as') where
	expand (x :. xs) = x :. expand xs

instance {-# OVERLAPPABLE #-} Expand b (a ':~ as) as' => Expand b (a ':~ as) (a' ':~ as') where
	expand xs = Nothing :. expand xs

class Project a (as :: Set Type) where
	prj :: UnionList b as -> Maybe a

instance Project a 'Nil where
	prj _ = Nothing

instance Project a (a ':~ as) where
	prj (x :. _) = x

instance {-# OVERLAPPABLE #-} Project a as => Project a (a' ':~ as) where
	prj (_ :. xs) = prj xs

extract :: HasCallStack => UnionList 'True (Singleton a) -> a
extract u = case prj u of Just x -> x; Nothing -> error "never occur"

class Collapse0 (as :: Set Type) (as' :: Set Type) where
	collapse0 :: UnionList b as -> UnionList b as'

instance {-# INCOHERENT #-} Nihil as' => Collapse0 'Nil as' where collapse0 = const nihil
instance {-# INCOHERENT #-} Collapse0 as 'Nil where collapse0 = const UnionListNil

instance Collapse0 as as' => Collapse0 (a ':~ as) (a ':~ as') where
	collapse0 (x :. xs) = x :. collapse0 xs

instance {-# OVERLAPPABLE #-} Collapse0 as as' => Collapse0 (a ':~ as) as' where
	collapse0 (_ :. xs) = collapse0 xs

class Collapsable b (as :: Set Type) (as' :: Set Type) where
	collapse :: UnionList b as -> Maybe (UnionList b as')

instance Collapse0 as as' => Collapsable 'False as as' where
	collapse = Just . collapse0

instance {-# OVERLAPPABLE #-} Collapsable 'True 'Nil as' where
	collapse = const Nothing

instance (Collapse0 as as', Collapsable 'True as as') => Collapsable 'True (a ':~ as) (a ':~ as') where
	collapse = \case
		(Just x :. xs) -> Just $ Just x :. collapse0 xs
		(Nothing :. xs) -> (Nothing :.) <$> collapse xs

instance {-# OVERLAPPABLE #-} Collapsable 'True as as' => Collapsable 'True (a ':~ as) as' where
	collapse (_ :. xs) = collapse xs

class Mergeable (as :: Set Type) (as' :: Set Type) (merged :: Set Type) where
	merge_ :: UnionList b as -> UnionList b as' -> UnionList b merged

instance Mergeable 'Nil 'Nil 'Nil where
	merge_ UnionListNil UnionListNil = UnionListNil

instance (Ord a, Mergeable as as' merged) => Mergeable (a ':~ as) (a ':~ as') (a ':~ merged) where
	merge_ (Just x :. xs) (Just x' :. xs') = Just (x `min` x') :. merge_ xs xs'
	merge_ (mx :. xs) (Nothing :. xs') = mx :. merge_ xs xs'
	merge_ (Nothing :. xs) (mx' :. xs') = mx' :. merge_ xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' merged => Mergeable (a ':~ as) as'  (a ':~ merged) where
	merge_ (x :. xs) xs' = x :. merge_ xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' merged => Mergeable as (a ':~ as') (a ':~ merged) where
	merge_ xs (x :. xs') = x :. merge_ xs xs'

merge :: Mergeable as as' (as :+: as') => UnionList b as -> UnionList b as' -> UnionList b (as :+: as')
merge = merge_
	
numbered [t| Bool |]
numbered [t| Char |]
numbered [t| Integer |]
numbered [t| Double |]

mergeMaybes :: (
	Mergeable es es' merged, Expand 'True es merged, Expand 'True es' merged
	) =>
	Maybe (UnionList 'True es) -> Maybe (UnionList 'True es') -> Maybe (UnionList 'True merged)
Just u `mergeMaybes` Just u' = Just $ u `merge_` u'
Just u `mergeMaybes` Nothing = Just $ expand u
Nothing `mergeMaybes` Just u' = Just $ expand u'
Nothing `mergeMaybes` Nothing = Nothing
