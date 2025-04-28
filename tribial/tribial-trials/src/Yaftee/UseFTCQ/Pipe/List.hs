{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.UseFTCQ.Pipe.List where

import Yaftee.UseFTCQ.Pipe qualified as Pipe

from xs = Pipe.yield `traverse` xs
