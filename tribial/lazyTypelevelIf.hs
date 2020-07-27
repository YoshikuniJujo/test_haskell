{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, PolyKinds #-}

-- type a >-> b = (b -> Type) -> a -> Type
type a >-> b = (b -> *) -> a -> *
type family ($) (f :: a >-> b) (x :: a) :: b

data BOOL :: (() >-> k) -> (() >-> k) -> (Bool >-> k)
type instance (BOOL f _) $ 'False = f $ '()
type instance (BOOL _ t) $ 'True = t $ '()
