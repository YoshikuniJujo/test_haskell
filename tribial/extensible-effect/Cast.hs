module Cast (cast1) where

import Data.Typeable

newtype Id x = Id x

cast1 :: (Typeable t, Typeable t') => t a -> Maybe (t' a)
cast1 x = fmap (\(Id x) -> x) $ gcast1 (Id x)
