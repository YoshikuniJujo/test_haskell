{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Png.Chunk.Encode (

	-- * RUN

	Chunk.encodeRun_, Chunk.EncodeStates,

	-- * ENCODE

	encode, C(..), Chunk.EncodeMembers

	) where

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
	Eff.E es C BSF.ByteString ()
encode nm = void $ convert Pipe.=$= Chunk.encode nm

convert :: U.Member Pipe.P es => Eff.E es C Chunk.C r
convert = forever $ Pipe.await >>= \f -> let C nm bd = f in
	mapM_ Pipe.yield [Chunk.Begin (olength bd) nm, Chunk.Body bd, Chunk.End]

data C = C { name :: BSF.ByteString, body :: BSF.ByteString }
	deriving Show
