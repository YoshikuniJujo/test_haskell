module Some where

import Prelude

foreign import unsafeHead :: forall a . Array a -> a

foreign import data Undefined :: Type -> Type

foreign import head :: forall a . Array a -> Undefined a

foreign import isUndefined :: forall a . Undefined a -> Boolean

isEmpty :: forall a . Array a -> Boolean
isEmpty = isUndefined <<< head
