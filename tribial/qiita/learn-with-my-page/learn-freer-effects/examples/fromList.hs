{-# LANGUAGE ExistentialQuantification #-}

infixr 9 :.:

data FunList a b = Fun (a -> b) | forall x . (x -> b) :.: FunList a x

apply :: FunList a b -> a -> b
apply (Fun f) = f
apply (f :.: fs) = f . apply fs
