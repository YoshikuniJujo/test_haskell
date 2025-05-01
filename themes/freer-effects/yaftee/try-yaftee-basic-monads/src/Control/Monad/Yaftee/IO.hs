{-# LANGUAGE ImportQualifiedPost #-}

module Control.Monad.Yaftee.IO where

import Control.HigherOpenUnion qualified as Union

type I = Union.FromFirst IO
