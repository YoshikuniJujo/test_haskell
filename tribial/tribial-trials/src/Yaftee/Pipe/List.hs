{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.Pipe.List where

import Yaftee.NewPipe qualified as Pipe

from xs = Pipe.yield `traverse` xs
