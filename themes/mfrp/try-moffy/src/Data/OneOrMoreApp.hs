{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.OneOrMoreApp (
	OneOrMoreApp, ExpandableApp, CollapsableApp, MergeableApp,
	pattern SingletonApp, unSingletonApp,
	collapseApp, (>-^), expandApp,
	mergeApp, mergeApp', projectApp
	) where

import Data.Kind
import Data.Type.Set.Internal
import Data.Type.SetApp.Internal
import qualified Data.OneOrMore.Internal as Oom

data OneOrMoreApp :: SetApp Type -> Type where
	OneOrMoreApp ::  Oom.OneOrMore as ->  OneOrMoreApp ('SetApp f as)

{-# COMPLETE SingletonApp #-}

pattern SingletonApp :: f a -> OneOrMoreApp ('SetApp f (f :$: (Singleton a)))
pattern SingletonApp x = OneOrMoreApp (Oom.Singleton x)

unSingletonApp :: OneOrMoreApp ('SetApp f (f :$: (Singleton a))) -> f a
unSingletonApp (SingletonApp x) = x

expandApp :: Oom.Expandable as as' => OneOrMoreApp ('SetApp f as) -> OneOrMoreApp ('SetApp f as')
expandApp (OneOrMoreApp xs) = OneOrMoreApp $ Oom.expand xs

collapseApp :: Oom.Collapsable as as' => OneOrMoreApp ('SetApp f as) -> Maybe (OneOrMoreApp ('SetApp f as'))
collapseApp (OneOrMoreApp xs) = OneOrMoreApp <$> Oom.collapse xs

projectApp :: Oom.Projectable as a => OneOrMoreApp ('SetApp f as) -> Maybe a
projectApp (OneOrMoreApp xs) = Oom.project xs

(>-^) :: Oom.Insertable a as as' => a -> OneOrMoreApp ('SetApp f as) -> OneOrMoreApp ('SetApp f as')
x >-^ (OneOrMoreApp xs) = OneOrMoreApp $ x Oom.>- xs

mergeApp :: Oom.Mergeable as as' mrg => OneOrMoreApp ('SetApp f as) -> OneOrMoreApp ('SetApp f as') -> OneOrMoreApp ('SetApp f mrg)
mergeApp (OneOrMoreApp xs) (OneOrMoreApp xs') = OneOrMoreApp $ xs `Oom.merge` xs'

unOneOrMoreApp :: OneOrMoreApp ('SetApp f as) -> Oom.OneOrMore as
unOneOrMoreApp (OneOrMoreApp xs) = xs

mergeApp' :: (Oom.Mergeable as as' mrg, Oom.Expandable as mrg, Oom.Expandable as' mrg) =>
	Maybe (OneOrMoreApp ('SetApp f as)) -> Maybe (OneOrMoreApp ('SetApp f as')) -> Maybe (OneOrMoreApp ('SetApp f mrg))
xs `mergeApp'` xs' = OneOrMoreApp <$> (unOneOrMoreApp <$> xs) `Oom.merge'` (unOneOrMoreApp <$> xs')

type ExpandableApp f as as' = Oom.Expandable (f :$: as) (f :$: as')
type CollapsableApp f as as' = Oom.Collapsable (f :$: as) (f :$: as')
type MergeableApp f as as' mrg = Oom.Mergeable (f :$: as) (f :$: as') (f :$: mrg)
