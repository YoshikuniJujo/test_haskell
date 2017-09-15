module Cast (cast1) where

import Data.Typeable (Typeable, gcast1)

newtype Id x = Id { unId :: x }

cast1 :: (Typeable t, Typeable t') => t a -> Maybe (t' a)
cast1 = (unId <$>) . gcast1 . Id
