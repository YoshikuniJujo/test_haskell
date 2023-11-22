{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses,
	FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.OneOrMore.Internal (
	-- * Type
	OneOrMore,
	-- * Property
	-- ** Basic Property
	Projectable, Insertable,
	-- ** Expandable and Collapsable
	Expandable, Collapsable,
	-- ** Mergeable
	Mergeable, Selectable(..),
	-- * Function
	-- ** Single Type
	pattern Singleton, unSingleton,
	-- ** Multiple Type
	project, (>-), (>-.),
	-- ** Expand and Collapse
	expand, collapse,
	-- ** Merge
	merge, merge', merge_, merge_' ) where

import Data.Kind (Type)
import Data.Type.Set.Internal (Set(Nil, (:~)), Singleton, (:-), (:+:))

---------------------------------------------------------------------------

-- * ONE OR MORE TYPE
-- * BASIC PROPERTY
--	+ PROJECTABLE
--	+ INSERTABLE
-- * EXPANDABLE AND COLLAPSABLE
--	+ EXPANDABLE
--	+ COLLAPSABLE
--		- COLLAPSABLE 0
--		- COLLAPSABLE
-- * MERGEABLE

---------------------------------------------------------------------------
-- ONE OR MORE TYPE
---------------------------------------------------------------------------

data OneOrMore :: Set Type -> Type where
	Empty :: OneOrMore 'Nil
	(:.) :: Maybe a -> OneOrMore as -> OneOrMore (a ':~ as)

---------------------------------------------------------------------------
-- BASIC PROPERTY
---------------------------------------------------------------------------

-- PROJECTABLE

class Projectable (as :: Set Type) a where project :: OneOrMore as -> Maybe a
instance Projectable 'Nil a where project _ = Nothing
instance Projectable (a ':~ as) a where project (x :. _) = x
instance {-# OVERLAPPABLE #-} Projectable as a =>
	Projectable (a' ':~ as) a where project (_ :. xs) = project xs

{-# COMPLETE Singleton #-}

pattern Singleton :: a -> OneOrMore (Singleton a)
pattern Singleton x = Just x :. Empty

unSingleton :: OneOrMore (Singleton a) -> a
unSingleton (Singleton x) = x

-- INSERTABLE

infixr 5 >-

class Insertable a (as :: Set Type) (as' :: Set Type) where
	(>-.) :: a -> OneOrMore as -> OneOrMore as'

instance Insertable a as (a ':~ as) where x >-. xs = Just x :. xs

instance {-# OVERLAPPABLE #-} Insertable a as as' =>
	Insertable a (a' ':~ as) (a' ':~ as') where
	x >-. (y :. xs) = y :. (x >-. xs)

(>-) :: Insertable a as (a :- as) => a -> OneOrMore as -> OneOrMore (a :- as)
(>-) = (>-.)

---------------------------------------------------------------------------
-- EXPANDABLE AND COLLAPSABLE
---------------------------------------------------------------------------

-- EXPANDABLE

class Expandable (as :: Set Type) (as' :: Set Type) where
	expand :: OneOrMore as -> OneOrMore as'

instance Nihil as => Expandable (a ':~ 'Nil) (a ':~ as) where
	expand (x :. Empty) = x :. nihil

instance {-# OVERLAPPABLE #-} Expandable as as' =>
	Expandable (a ':~ as) (a ':~ as') where
	expand (x :. xs) = x :. expand xs

instance {-# OVERLAPPABLE #-} Expandable (a ':~ as) as' =>
	Expandable (a ':~ as) (a' ':~ as') where
	expand xs = Nothing :. expand xs

class Nihil as where nihil :: OneOrMore as
instance Nihil 'Nil where nihil = Empty
instance Nihil as => Nihil (a ':~ as) where nihil = Nothing :. nihil

-- COLLAPSABLE

-- COLLAPSABLE 0

class Collapsable0 (as :: Set Type) (as' :: Set Type) where
	collapse0 :: OneOrMore as -> OneOrMore as'

instance Collapsable0 as 'Nil where collapse0 = const Empty

instance {-# OVERLAPPABLE #-} Collapsable0 as as' =>
	Collapsable0 (a ':~ as) (a ':~ as') where
	collapse0 (x :. xs) = x :. collapse0 xs

instance {-# OVERLAPPABLE #-} Collapsable0 as (a' ':~ as') =>
	Collapsable0 (a ':~ as) (a' ':~ as') where
	collapse0 (_ :. xs) = collapse0 xs

-- COLLAPSABLE

class Collapsable (as :: Set Type) (as' :: Set Type) where
	collapse :: OneOrMore as -> Maybe (OneOrMore as')

instance Collapsable 'Nil 'Nil where collapse = const Nothing

instance (Collapsable0 as as', Collapsable as as') =>
	Collapsable (a ':~ as) (a ':~ as') where
	collapse = \case
		Just x :. xs -> Just $ Just x :. collapse0 xs
		Nothing :. xs -> (Nothing :.) <$> collapse xs

instance {-# OVERLAPPABLE #-} Collapsable as as' =>
	Collapsable (a ':~ as) as' where
	collapse (_ :. xs) = collapse xs

---------------------------------------------------------------------------
-- MERGEABLE
---------------------------------------------------------------------------

class Mergeable (as :: Set Type) (as' :: Set Type) (mrg :: Set Type) where
	merge_ :: OneOrMore as -> OneOrMore as' -> OneOrMore mrg

instance Mergeable 'Nil 'Nil 'Nil where merge_ Empty Empty = Empty

instance (Selectable a, Mergeable as as' mrg) =>
	Mergeable (a ':~ as) (a ':~ as') (a ':~ mrg) where
	merge_ (Just x :. xs) (Just x' :. xs') =
		Just (x `select` x') :. merge_ xs xs'
	merge_ (mx :. xs) (Nothing :. xs') = mx :. merge_ xs xs'
	merge_ (Nothing :. xs) (mx' :. xs') = mx' :. merge_ xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' mrg =>
	Mergeable (a ':~ as) as'  (a ':~ mrg) where
	merge_ (x :. xs) xs' = x :. merge_ xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' mrg =>
	Mergeable as (a ':~ as') (a ':~ mrg) where
	merge_ xs (x :. xs') = x :. merge_ xs xs'

class Selectable a where select :: a -> a -> a
instance {-# OVERLAPPABLE #-} Ord a => Selectable a where select = min

merge_' :: (Mergeable as as' mrg, Expandable as mrg, Expandable as' mrg ) =>
	Maybe (OneOrMore as) -> Maybe (OneOrMore as') -> Maybe (OneOrMore mrg)
ml `merge_'` mr = case (ml, mr) of
	(Just l, Just r) -> Just $ l `merge_` r
	(Just l, Nothing) -> Just $ expand l
	(Nothing, Just r) -> Just $ expand r
	(Nothing, Nothing) -> Nothing

merge :: Mergeable as as' (as :+: as') =>
	OneOrMore as -> OneOrMore as' -> OneOrMore (as :+: as')
merge = merge_

merge' :: (
	Mergeable as as' (as :+: as'),
	Expandable as (as :+: as'), Expandable as' (as :+: as') ) =>
	Maybe (OneOrMore as) -> Maybe (OneOrMore as') -> Maybe (OneOrMore (as :+: as'))
merge' = merge_'
