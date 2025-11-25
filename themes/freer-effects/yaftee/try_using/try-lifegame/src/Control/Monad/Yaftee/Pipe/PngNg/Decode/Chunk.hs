{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll, RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.PngNg.Decode.Chunk (chunks, Chunk(..)) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Png.Chunk
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Bytes
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.ByteString.FingerTree qualified as BSF

chunks :: forall nm -> (
	U.Member Pipe.P es, Bytes.BytesMembers nm es,
	U.Member (Except.E String) es ) => Eff.E es BSF.ByteString Chunk ()
chunks = decode
