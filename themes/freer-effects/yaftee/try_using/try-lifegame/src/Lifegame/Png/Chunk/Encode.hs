{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Png.Chunk.Encode (encode, Chunk(..)) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as Chunk
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.MonoTraversable
import Data.ByteString.FingerTree qualified as BSF

encode :: forall nm -> (
	U.Member Pipe.P es, Chunk.EncodeMembers nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	a -> Eff.E es (a -> (Chunk, a)) BSF.ByteString ()
encode nm st0 = void $ convert st0 Pipe.=$= Chunk.encode nm

convert :: U.Member Pipe.P es => a -> Eff.E es (a -> (Chunk, a)) Chunk.C r
convert st0 =
	fvr st0 \st -> Pipe.await >>= \f -> let (Chunk nm bd, st') = f st in
	st' <$ mapM_ Pipe.yield
		[Chunk.Begin (olength bd) nm, Chunk.Body bd, Chunk.End]
	where fvr st a = a st >>= (`fvr` a)

data Chunk = Chunk { name :: BSF.ByteString, body :: BSF.ByteString }
	deriving Show
