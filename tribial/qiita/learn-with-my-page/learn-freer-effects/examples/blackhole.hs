{-# LANGUAGE ExistentialQuantification #-}

data Blackhole = forall x . Blackhole x
