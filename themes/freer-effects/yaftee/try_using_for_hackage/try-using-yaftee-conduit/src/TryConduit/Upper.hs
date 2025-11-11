{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryConduit.Upper where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.HigherOpenUnion qualified as U
import Data.Char

upper :: U.Member Pipe.P es => Eff.E es String String r
upper = PipeT.convert (toUpper <$>)

