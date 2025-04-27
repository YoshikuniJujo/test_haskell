{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.Legacy.Pipe.List where

import Yaftee.Legacy.NewPipe qualified as Pipe

from xs = Pipe.yield `traverse` xs
