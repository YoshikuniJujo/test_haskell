{-# LANGUAGE ExistentialQuantification #-}

module UnionNoType (Union, toUnion, fromUnion) where

import Data.Typeable

data Union a = forall t . Typeable t => U (t a)

toUnion :: Typeable t => t a -> Union a
toUnion = U

fromUnion :: Typeable t => Union a -> Maybe (t a)
fromUnion (U ta) = cast1 ta

newtype Id x = Id { unId :: x }

cast1 :: (Typeable t, Typeable t') => t a -> Maybe (t' a)
cast1 = (unId <$>) . gcast1 . Id
