module LifeTypeable where

import Prelude
import Data.Generic.Rep
import Data.Eq
import Data.Show
import Data.Show.Generic
import Data.Maybe
import Unsafe.Coerce

newtype LifeType = LifeType String

derive instance Generic LifeType _
derive instance Eq LifeType

instance Show LifeType where show = genericShow

class LifeTypeable a where lifeTypeOf :: LifeType

cast :: forall a b . LifeTypeable a => LifeTypeable b => a -> Maybe b
cast x  | lifeTypeOf @a == lifeTypeOf @b = Just $ unsafeCoerce x
        | otherwise = Nothing
