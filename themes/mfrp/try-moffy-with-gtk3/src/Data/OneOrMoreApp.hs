{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.OneOrMoreApp (
	-- * Type
	OneOrMoreApp,
	-- * Constraint Synonym
	Expandable, Collapsable, Mergeable,
	-- * Function
	-- ** Single Type
	pattern Singleton, unSingleton,
	-- ** Multiple Type
	project, (>-),
	-- ** Expand and Collapse
	expand, collapse,
	-- ** Merge
	merge, merge' ) where

import Data.Kind
import Data.Type.Set.Internal
import Data.Type.SetApp.Internal
import qualified Data.OneOrMore.Internal as Oom

data OneOrMoreApp :: SetApp Type -> Type where
	OneOrMoreApp ::  Oom.OneOrMore as ->  OneOrMoreApp ('SetApp f as)

{-# COMPLETE Singleton #-}

pattern Singleton :: a -> OneOrMoreApp ('SetApp f (Singleton a))
pattern Singleton x = OneOrMoreApp (Oom.Singleton x)

unSingleton :: OneOrMoreApp ('SetApp f (Singleton a)) -> a
unSingleton (Singleton x) = x

expand :: Oom.Expandable as as' => OneOrMoreApp ('SetApp f as) -> OneOrMoreApp ('SetApp f as')
expand (OneOrMoreApp xs) = OneOrMoreApp $ Oom.expand xs

collapse :: Oom.Collapsable as as' => OneOrMoreApp ('SetApp f as) -> Maybe (OneOrMoreApp ('SetApp f as'))
collapse (OneOrMoreApp xs) = OneOrMoreApp <$> Oom.collapse xs

project :: Oom.Projectable as a => OneOrMoreApp ('SetApp f as) -> Maybe a
project (OneOrMoreApp xs) = Oom.project xs

(>-) :: Oom.Insertable a as as' => a -> OneOrMoreApp ('SetApp f as) -> OneOrMoreApp ('SetApp f as')
x >- (OneOrMoreApp xs) = OneOrMoreApp $ x Oom.>-. xs

merge :: Oom.Mergeable as as' mrg => OneOrMoreApp ('SetApp f as) -> OneOrMoreApp ('SetApp f as') -> OneOrMoreApp ('SetApp f mrg)
OneOrMoreApp xs `merge` OneOrMoreApp xs' = OneOrMoreApp $ xs `Oom.merge_` xs'

unOneOrMoreApp :: OneOrMoreApp ('SetApp f as) -> Oom.OneOrMore as
unOneOrMoreApp (OneOrMoreApp xs) = xs

merge' :: (Oom.Mergeable as as' mrg, Oom.Expandable as mrg, Oom.Expandable as' mrg) =>
	Maybe (OneOrMoreApp ('SetApp f as)) -> Maybe (OneOrMoreApp ('SetApp f as')) -> Maybe (OneOrMoreApp ('SetApp f mrg))
xs `merge'` xs' = OneOrMoreApp <$> (unOneOrMoreApp <$> xs) `Oom.merge_'` (unOneOrMoreApp <$> xs')

type Expandable f as as' = Oom.Expandable (f `Map` as) (f `Map` as')
type Collapsable f as as' = Oom.Collapsable (f `Map` as) (f `Map` as')
type Mergeable f as as' mrg = Oom.Mergeable (f `Map` as) (f `Map` as') (f `Map` mrg)
