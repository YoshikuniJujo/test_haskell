module Control.Monad.Freer.Failable (F(..)) where

class F t where fail :: String -> t a
