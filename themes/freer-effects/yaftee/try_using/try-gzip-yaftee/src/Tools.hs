module Tools where

import Data.Bool

whenDef :: Applicative m => a -> Bool -> m a -> m a
whenDef d b a = bool (pure d) a b

whenMaybe :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenMaybe mx f = maybe (pure ()) f mx

whenMaybeDef :: Applicative m => b -> Maybe a -> (a -> m b) -> m b
whenMaybeDef d mx f = maybe (pure d) f mx
