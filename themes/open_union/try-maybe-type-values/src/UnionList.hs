{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module UnionList (
	UnionList(UnionListNil), AddValue, MinValue, Expand,
	(>+), (>-), expand) where

import Data.Kind
import Sorted.Internal

data UnionList :: Sorted Type -> * where
	UnionListNil :: UnionList 'Nil
	(:.) :: Maybe a -> UnionList as -> UnionList (a ':~ as)

class AddValue a (as :: Sorted Type) (as' :: Sorted Type) where
	(>+.) :: a -> UnionList as -> UnionList as'

instance AddValue a as (a ':~ as) where
	x >+. xs = Just x :. xs

instance {-# OVERLAPPABLE #-} AddValue a as as' => AddValue a (a' ':~ as) (a' ':~ as') where
	x >+. (y :. xs) = y :. (x >+. xs)

infixr 5 >+

(>+) :: AddValue a as (a :- as) => a -> UnionList as -> UnionList (a :- as)
(>+) = (>+.)

class MinValue a (as :: Sorted Type) (as' :: Sorted Type) where
	(>-.) :: Ord a => a -> UnionList as -> UnionList as'

instance MinValue a (a ':~ as) (a ':~ as) where
	x >-. (x' :. xs) = Just (maybe x (x `min`) x') :. xs

instance {-# OVERLAPPABLE #-} MinValue a as (a ':~ as) where
	x >-. xs = Just x :. xs

instance {-# OVERLAPPABLE #-} MinValue a as as' => MinValue a (a' ':~ as) (a' ':~ as') where
	x >-. (y :. xs) = y :. (x >-. xs)

infixr 5 >-

(>-) :: (MinValue a as (a :- as), Ord a) => a -> UnionList as -> UnionList (a :- as)
(>-) = (>-.)

class Nihil as where nihil :: UnionList as

instance Nihil 'Nil where nihil = UnionListNil

instance Nihil as => Nihil (a ':~ as) where
	nihil = Nothing :. nihil

class Expand (as :: Sorted Type) (as' :: Sorted Type) where
	expand :: UnionList as -> UnionList as'

instance Nihil as => Expand (a ':~ 'Nil) (a ':~ as) where
	expand (x :. UnionListNil) = x :. nihil

instance {-# OVERLAPPABLE #-} Expand as as' => Expand (a ':~ as) (a ':~ as') where
	expand (x :. xs) = x :. expand xs

instance {-# OVERLAPPABLE #-} Expand (a ':~ as) as' => Expand (a ':~ as) (a' ':~ as') where
	expand xs = Nothing :. expand xs

class Project a (as :: Sorted Type) where
	prj :: UnionList as -> Maybe a

instance Project a 'Nil where
	prj _ = Nothing

instance Project a (a ':~ as) where
	prj (x :. _) = x

instance {-# OVERLAPPABLE #-} Project a as => Project a (a' ':~ as) where
	prj (_ :. xs) = prj xs

numbered [t| Bool |]
numbered [t| Char |]
numbered [t| Integer |]
numbered [t| Double |]
