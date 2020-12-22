{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word

class Foo a where enumSize :: Integer

instance (Bounded a, Enum a) => Foo a where
	enumSize = fromEnumI @a maxBound - fromEnumI @a minBound + 1

enumSizeWord8 :: Integer
enumSizeWord8 = enumSize @Word8

fromEnumI :: Enum a => a -> Integer
fromEnumI = fromIntegral . fromEnum
