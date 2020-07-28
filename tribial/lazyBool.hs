{-# LANGUAGE Strict #-}

bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

lazyBool :: (() -> a) -> (() -> a) -> Bool -> a
lazyBool x _ False = x ()
lazyBool _ y True = y ()
