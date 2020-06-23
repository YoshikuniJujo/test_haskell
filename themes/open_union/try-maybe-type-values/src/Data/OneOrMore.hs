{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses,
	FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.OneOrMore (
	-- * Type
	OneOrMore(Empty),
	-- * Property
	-- ** Basic Property
	Projectable, Insertable,
	-- ** Expandable and Collapsable
	Expandable, Nihil, Collapsable,
	-- ** Mergeable
	Mergeable, Selectable(..),
	-- * FUNCTION
	prj, extract, singleton, (>-), expand, collapse, merge, merge' ) where

import Data.Kind (Type)
import Data.Type.Set.Internal (Set(..), Singleton)

---------------------------------------------------------------------------

-- * ONEORMORE TYPE
-- * BASIC PROPERTY
--	+ PROJECTABLE
--	+ INSERTABLE
-- * EXPANDABLE AND COLLAPSABLE
--	+ EXPANDABLE
--	+ COLLAPSABLE
-- * MERGEABLE

---------------------------------------------------------------------------
-- ONEORMORE TYPE
---------------------------------------------------------------------------

data OneOrMore :: Set Type -> Type where
	Empty :: OneOrMore 'Nil
	(:.) :: Maybe a -> OneOrMore as -> OneOrMore (a ':~ as)

---------------------------------------------------------------------------
-- BASIC PROPERTY
---------------------------------------------------------------------------

-- PROJECTABLE

class Projectable (as :: Set Type) a where prj :: OneOrMore as -> Maybe a
instance Projectable 'Nil a where prj _ = Nothing
instance Projectable (a ':~ as) a where prj (x :. _) = x
instance {-# OVERLAPPABLE #-} Projectable as a =>
	Projectable (a' ':~ as) a where prj (_ :. xs) = prj xs

extract :: OneOrMore (Singleton a) -> a
extract u = case prj u of Just x -> x; Nothing -> error "never occur"

-- INSERTABLE

infixr 5 >-

class Insertable a (as :: Set Type) (as' :: Set Type) where
	(>-) :: a -> OneOrMore as -> OneOrMore as'

instance Insertable a as (a ':~ as) where
	x >- xs = Just x :. xs

instance {-# OVERLAPPABLE #-} Insertable a as as' =>
	Insertable a (a' ':~ as) (a' ':~ as') where
	x >- (y :. xs) = y :. (x >- xs)

singleton :: a -> OneOrMore (Singleton a)
singleton = (>- Empty)

---------------------------------------------------------------------------
-- EXPANDABLE AND COLLAPSABLE
---------------------------------------------------------------------------

-- EXPANDABLE

class Nihil as where nihil :: OneOrMore as
instance Nihil 'Nil where nihil = Empty
instance Nihil as => Nihil (a ':~ as) where nihil = Nothing :. nihil

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

-- COLLAPSABLE

-- Collapsable0

class Collapsable0 (as :: Set Type) (as' :: Set Type) where
	collapse0 :: OneOrMore as -> OneOrMore as'

instance Collapsable0 as 'Nil where collapse0 = const Empty

instance {-# OVERLAPPABLE #-} Collapsable0 as as' =>
	Collapsable0 (a ':~ as) (a ':~ as') where
	collapse0 (x :. xs) = x :. collapse0 xs

instance {-# OVERLAPPABLE #-} Collapsable0 as (a' ':~ as') =>
	Collapsable0 (a ':~ as) (a' ':~ as') where
	collapse0 (_ :. xs) = collapse0 xs

-- Collapsable

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

class Selectable a where select :: a -> a -> a
instance {-# OVERLAPPABLE #-} Ord a => Selectable a where select = min

class Mergeable (as :: Set Type) (as' :: Set Type) (mrg :: Set Type) where
	merge :: OneOrMore as -> OneOrMore as' -> OneOrMore mrg

instance Mergeable 'Nil 'Nil 'Nil where merge Empty Empty = Empty

instance (Selectable a, Mergeable as as' mrg) =>
	Mergeable (a ':~ as) (a ':~ as') (a ':~ mrg) where
	merge (Just x :. xs) (Just x' :. xs') =
		Just (x `select` x') :. merge xs xs'
	merge (mx :. xs) (Nothing :. xs') = mx :. merge xs xs'
	merge (Nothing :. xs) (mx' :. xs') = mx' :. merge xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' mrg =>
	Mergeable (a ':~ as) as'  (a ':~ mrg) where
	merge (x :. xs) xs' = x :. merge xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' mrg =>
	Mergeable as (a ':~ as') (a ':~ mrg) where
	merge xs (x :. xs') = x :. merge xs xs'

merge' :: (Mergeable as as' mrg, Expandable as mrg, Expandable as' mrg ) =>
	Maybe (OneOrMore as) -> Maybe (OneOrMore as') -> Maybe (OneOrMore mrg)
mu `merge'` mu' = case (mu, mu') of
	(Just u, Just u') -> Just $ u `merge` u'
	(Just u, Nothing) -> Just $ expand u
	(Nothing, Just u') -> Just $ expand u'
	(Nothing, Nothing) -> Nothing
