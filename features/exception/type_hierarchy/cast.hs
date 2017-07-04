{-# LANGUAGE ScopedTypeVariables #-}

import Data.Typeable.Internal
import Unsafe.Coerce

cast :: forall a b . (Typeable a, Typeable b) => a -> Maybe b
cast x = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
	then Just $ unsafeCoerce x
	else Nothing
