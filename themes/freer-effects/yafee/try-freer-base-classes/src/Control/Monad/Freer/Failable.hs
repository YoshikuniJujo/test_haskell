module Control.Monad.Freer.Failable where

class F t where fail :: String -> t a
