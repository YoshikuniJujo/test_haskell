{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.OneOrMoreApp where

import Data.Kind
import Data.Type.Set.Internal
import Data.Type.SetApp.Internal
import qualified Data.OneOrMore.Internal as Oom

data OneOrMoreApp :: SetApp Type -> Type where
	OneOrMoreApp ::  Oom.OneOrMore as ->  OneOrMoreApp ('SetApp f as)

{-# COMPLETE Singleton #-}

pattern Singleton :: f a -> OneOrMoreApp ('SetApp f (f :$: (Singleton a)))
pattern Singleton x = OneOrMoreApp (Oom.Singleton x)

class ExpandableApp (as :: SetApp Type) (as' :: SetApp Type) where
	expandApp :: OneOrMoreApp as -> OneOrMoreApp as'

instance Oom.Expandable as as' =>
	ExpandableApp ('SetApp f as) ('SetApp f as') where
	expandApp (OneOrMoreApp xs) = OneOrMoreApp $ Oom.expand xs

class CollapseableApp (as :: SetApp Type) (as' :: SetApp Type) where
	collapseApp :: OneOrMoreApp as -> Maybe (OneOrMoreApp as')

instance Oom.Collapsable as as' =>
	CollapseableApp ('SetApp f as) ('SetApp f as') where
	collapseApp (OneOrMoreApp xs) = OneOrMoreApp <$> Oom.collapse xs

class ProjectableApp (as :: SetApp Type) a where
	projectApp :: OneOrMoreApp as -> Maybe a

instance Oom.Projectable as a => ProjectableApp ('SetApp f as) a where
	projectApp (OneOrMoreApp xs) = Oom.project xs
