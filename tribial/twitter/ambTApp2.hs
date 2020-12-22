{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

enumSize :: forall a . (Bounded a, Enum a) => Int
enumSize = fromEnum @a maxBound  - fromEnum @a minBound + 1
