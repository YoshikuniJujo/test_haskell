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
	Projectable, Nihil, Insertable, Expandable, Collapsable,
	Selectable(..), Mergeable,
	-- * FUNCTION
	prj, extract, singleton, (>-), expand, collapse, merge, merge' ) where

import Data.Kind
import Data.Type.Set.Internal hiding (Merge)

data OneOrMore :: Set Type -> Type where
	Empty :: OneOrMore 'Nil
	(:.) :: Maybe a -> OneOrMore as -> OneOrMore (a ':~ as)

infixr 5 >-

class Insertable a (as :: Set Type) (as' :: Set Type) where
	(>-) :: a -> OneOrMore as -> OneOrMore as'

instance Insertable a as (a ':~ as) where
	x >- xs = Just x :. xs

instance {-# OVERLAPPABLE #-} Insertable a as as' => Insertable a (a' ':~ as) (a' ':~ as') where
	x >- (y :. xs) = y :. (x >- xs)

singleton :: a -> OneOrMore (Singleton a)
singleton = (>- Empty)

class Nihil as where nihil :: OneOrMore as
instance Nihil 'Nil where nihil = Empty
instance Nihil as => Nihil (a ':~ as) where nihil = Nothing :. nihil

class Expandable (as :: Set Type) (as' :: Set Type) where
	expand :: OneOrMore as -> OneOrMore as'

instance Nihil as => Expandable (a ':~ 'Nil) (a ':~ as) where
	expand (x :. Empty) = x :. nihil

instance {-# OVERLAPPABLE #-} Expandable as as' => Expandable (a ':~ as) (a ':~ as') where
	expand (x :. xs) = x :. expand xs

instance {-# OVERLAPPABLE #-} Expandable (a ':~ as) as' => Expandable (a ':~ as) (a' ':~ as') where
	expand xs = Nothing :. expand xs

class Projectable (as :: Set Type) a where
	prj :: OneOrMore as -> Maybe a

instance Projectable 'Nil a where
	prj _ = Nothing

instance Projectable (a ':~ as) a where
	prj (x :. _) = x

instance {-# OVERLAPPABLE #-} Projectable as a => Projectable (a' ':~ as) a where
	prj (_ :. xs) = prj xs

extract :: OneOrMore (Singleton a) -> a
extract u = case prj u of Just x -> x; Nothing -> error "never occur"

class Collapsable0 (as :: Set Type) (as' :: Set Type) where
	collapse0 :: OneOrMore as -> OneOrMore as'

instance Collapsable0 as 'Nil where collapse0 = const Empty

instance {-# OVERLAPPABLE #-} Collapsable0 as as' => Collapsable0 (a ':~ as) (a ':~ as') where
	collapse0 (x :. xs) = x :. collapse0 xs

instance {-# OVERLAPPABLE #-} Collapsable0 as (a' ':~ as') => Collapsable0 (a ':~ as) (a' ':~ as') where
	collapse0 (_ :. xs) = collapse0 xs

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

class Selectable a where select :: a -> a -> a

instance {-# OVERLAPPABLE #-} Ord a => Selectable a where select = min

class Mergeable (as :: Set Type) (as' :: Set Type) (merged :: Set Type) where
	merge :: OneOrMore as -> OneOrMore as' -> OneOrMore merged

instance Mergeable 'Nil 'Nil 'Nil where merge Empty Empty = Empty

instance (Selectable a, Mergeable as as' merged) =>
	Mergeable (a ':~ as) (a ':~ as') (a ':~ merged) where
	merge (Just x :. xs) (Just x' :. xs') = Just (x `select` x') :. merge xs xs'
	merge (mx :. xs) (Nothing :. xs') = mx :. merge xs xs'
	merge (Nothing :. xs) (mx' :. xs') = mx' :. merge xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' merged =>
	Mergeable (a ':~ as) as'  (a ':~ merged) where
	merge (x :. xs) xs' = x :. merge xs xs'

instance {-# OVERLAPPABLE #-} Mergeable as as' merged =>
	Mergeable as (a ':~ as') (a ':~ merged) where
	merge xs (x :. xs') = x :. merge xs xs'

merge' :: (
	Mergeable as as' merged,
	Expandable as merged, Expandable as' merged ) =>
	Maybe (OneOrMore as) -> Maybe (OneOrMore as') -> Maybe (OneOrMore merged)
Just u `merge'` Just u' = Just $ u `merge` u'
Just u `merge'` Nothing = Just $ expand u
Nothing `merge'` Just u' = Just $ expand u'
Nothing `merge'` Nothing = Nothing
