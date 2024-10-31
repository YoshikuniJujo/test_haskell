{-# LANGUAGE BlockArguments #-}

module Control.Monad.Cont.MiscYj where

import Control.Monad.Cont

mapContM :: (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
mapContM f = runContT . mapM (ContT . f)
