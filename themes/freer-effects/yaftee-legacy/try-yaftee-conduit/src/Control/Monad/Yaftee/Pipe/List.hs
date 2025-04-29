{-# LANGUAGE ImportQualifiedPost #-}

module Control.Monad.Yaftee.Pipe.List where

import Control.Monad.Yaftee.Pipe qualified as Pipe

from xs = Pipe.yield `traverse` xs
