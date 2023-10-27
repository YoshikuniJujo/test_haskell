{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.OneOfThem (
	-- * OneOfThem
	-- ** Type
	OneOfThem,
	-- ** Single
	pattern Singleton, unSingleton,
	-- ** Multiple
	-- *** Project
	Projectable, project,
	-- *** Expand
	Expandable, (>-), expand,
	-- * OneOfThemFun
	-- ** Type and Apply
	OneOfThemFun, apply,
	-- ** Single
	pattern SingletonFun,
	-- ** Insert
	InsertableFun, (>--),
	-- ** Merge
	MergeableFun, mergeFun
	) where

import Data.Kind (Type)
import Data.Type.Set.Internal -- (Set(Nil, (:~)), Singleton)

data OneOfThem :: Set Type -> Type where
	JustIt :: a -> OneOfThem (a ':~ as)
	Wrap :: OneOfThem as -> OneOfThem (a ':~ as)

instance Show (OneOfThem 'Nil) where
	show _ = error "bad"

instance (Show a, Show (OneOfThem as)) => Show (OneOfThem (a :~ as)) where
	show (JustIt x) = "(JustIt " ++ show x ++ ")"
	show (Wrap xs) = show xs

{-# COMPLETE Singleton #-}

pattern Singleton :: a -> OneOfThem (Singleton a)
pattern Singleton x = JustIt x

unSingleton :: OneOfThem (Singleton a) -> a
unSingleton (Singleton x) = x

class Expandable (as :: Set Type) (as' :: Set Type) where
	expand :: OneOfThem as -> OneOfThem as'

instance Expandable 'Nil as where expand _ = error "never occur"

instance Expandable as as' => Expandable (a ':~ as) (a ':~ as') where
	expand (JustIt x) = JustIt x
	expand (Wrap oot) = Wrap $ expand oot

instance {-# OVERLAPPABLE #-} Expandable (a ':~ as) as' =>
	Expandable (a ':~ as) (a' ':~ as') where
	expand x = Wrap $ expand x

class Projectable (as :: Set Type) a where project :: OneOfThem as -> Maybe a

instance Projectable 'Nil a where project _ = Nothing
instance Projectable (a ':~ as) a where
	project (JustIt x) = Just x
	project (Wrap _) = Nothing
instance {-# OVERLAPPABLE #-} Projectable as a =>
	Projectable (a' ':~ as) a where
	project (JustIt _) = Nothing
	project (Wrap xs) = project xs

class Collapsable (as :: Set Type) (as' :: Set Type) where
	collapse :: OneOfThem as -> Maybe (OneOfThem as')

instance Collapsable as 'Nil where collapse _ = Nothing

instance Collapsable as as' => Collapsable (a ':~ as) (a ':~ as') where
	collapse (JustIt x) = Just $ JustIt x
	collapse (Wrap oot) = Wrap <$> collapse oot

instance {-# OVERLAPPABLE #-} Collapsable as (a' ':~ as') =>
	Collapsable (a ':~ as) (a' ':~ as') where
	collapse (JustIt _) = Nothing
	collapse (Wrap oot) = collapse oot

data OneOfThemFun (as :: Set Type) b where
	EmptyFun :: OneOfThemFun 'Nil b
	(:..) :: (a -> b) -> OneOfThemFun as b -> OneOfThemFun (a ':~ as) b

class InsertableFun a (as :: Set Type) (as' :: Set Type) where
	(>--.) :: (a -> b) -> OneOfThemFun as b -> OneOfThemFun as' b

instance InsertableFun a as (a ':~ as) where f >--. fs = f :.. fs

instance {-# OVERLAPPABLE #-} InsertableFun a as as' =>
	InsertableFun a (a' ':~ as) (a' ':~ as') where
	f >--. (g :.. fs) = g :.. (f >--. fs)

infixr 5 >-, >--

(>--) :: InsertableFun a as (a :- as) => (a -> b) -> OneOfThemFun as b -> OneOfThemFun (a :- as) b
(>--) = (>--.)

{-# COMPLETE SingletonFun #-}

pattern SingletonFun :: (a -> b) -> OneOfThemFun (Singleton a) b
pattern SingletonFun f = f :.. EmptyFun

class Applyable as where
	apply :: OneOfThemFun as b -> OneOfThem as -> b

instance Applyable (Singleton a) where
	apply (SingletonFun f) (Singleton x) = f x

instance {-# OVERLAPPABLE #-} Applyable as => Applyable (a ':~ as) where
	apply (f :.. _) (JustIt x) = f x
	apply (_ :.. fs) (Wrap xs) = fs `apply` xs

(>-) :: (Expandable (Singleton a) (a :- as), Expandable as (a :- as)) => a -> [OneOfThem as] -> [OneOfThem (a :- as)]
x >- xs = expand (Singleton x) : (expand <$> xs)

class MergeableFun as as' mrg where
	mergeFun_ :: OneOfThemFun as b -> OneOfThemFun as' b -> OneOfThemFun mrg b

instance MergeableFun 'Nil 'Nil 'Nil where
	mergeFun_ EmptyFun EmptyFun = EmptyFun

instance MergeableFun as as' mrg => MergeableFun as (a' ':~ as') (a' ':~ mrg) where
	mergeFun_ fs (g :.. gs) = g :.. mergeFun_ fs gs

instance MergeableFun as as' mrg => MergeableFun (a ':~ as) as' (a ':~ mrg) where
	mergeFun_ (f :.. fs) gs = f :.. mergeFun_ fs gs

mergeFun :: MergeableFun as as' (as :+: as') =>
	OneOfThemFun as b -> OneOfThemFun as' b -> OneOfThemFun (as :+: as') b
mergeFun = mergeFun_
