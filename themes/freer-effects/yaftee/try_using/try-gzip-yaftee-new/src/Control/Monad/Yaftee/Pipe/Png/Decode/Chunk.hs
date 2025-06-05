{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Chunk (

	chunkRun_, ChunkStates,
	chunk, ChunkMembers,

	Chunk(..),

	readBytes, ByteString, Crc32

	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Foldable
import Data.Bool
import Data.Int
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS

chunkRun_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates nm `Append` es) i o r ->
	Eff.E es i o ()
chunkRun_ = void
	. (`State.runN` Crc32 Crc.initialCrc32)
	. (`State.runN` ByteString "")
	. (`State.runN` Chunk "IHDR")

type ChunkStates nm =
	'[State.Named nm Chunk, State.Named nm ByteString, State.Named nm Crc32]

type ChunkMembers nm es = (
	U.Member (State.Named nm Chunk) es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es )

chunk :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (State.Named nm ByteString) es,
	U.Member (Except.E String) es
	) =>
	Int64 -> Eff.E es LBS.ByteString LBS.ByteString ()
chunk nm = doWhile_ . chunk1 nm

chunk1 :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	ChunkMembers nm es,
	U.Member (Except.E String) es
	) =>
	Int64 -> Eff.E es LBS.ByteString LBS.ByteString Bool
chunk1 nm m = do
	n <- LBS.toBitsBE <$> readBytes nm 4
	resetCrc32 nm
	cn <- readBytes nm 4
	State.putN nm $ Chunk cn
--	Pipe.yield . Left $ ChunkBegin cn
--	for_ (split m n) \n' -> Pipe.yield =<< Right <$> readBytes nm n'
	for_ (split m n) \n' -> Pipe.yield =<< readBytes nm n'
	compCrc32 nm
	Crc32 crc1 <- State.getN nm
	crc0 <- Crc.crc32FromByteStringBE <$> readBytes nm 4
	when (Just crc1 /= crc0) $ Except.throw @String "chunk1: CRC32 error"
--	Pipe.yield . Left $ ChunkEnd cn
	pure $ cn /= "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m'	| n < m' -> n : go (m' - n)
			| otherwise -> [m']

readBytes :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (Except.E String) es ) =>
	Int64 -> Eff.E es LBS.ByteString o LBS.ByteString
readBytes nm n = State.getsN nm (LBS.splitAt' n . unByteString) >>= \case
	Nothing -> readMore nm
		>>= bool (Except.throw @String "no more ByteString") (readBytes nm n)
	Just (t, d) -> t <$ do
		State.modifyN nm $ Crc32 . (`Crc.crc32StepBS'` t) . unCrc32
		State.putN nm (ByteString d)

resetCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
resetCrc32 nm = State.putN nm $ Crc32 Crc.initialCrc32

compCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
compCrc32 nm = State.modifyN nm $ Crc32 . Crc.complementCrc32 . unCrc32

readMore :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Eff.E es LBS.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm (`appendByteString` bs)

newtype ByteString = ByteString { unByteString :: LBS.ByteString } deriving Show
newtype Crc32 = Crc32 { unCrc32 :: Crc.Crc32 } deriving Show

appendByteString :: ByteString -> LBS.ByteString -> ByteString
appendByteString (ByteString bs1) bs2 = ByteString $ bs1 `LBS.append` bs2

data ChunkTag = ChunkBegin LBS.ByteString | ChunkEnd LBS.ByteString deriving Show

newtype Chunk = Chunk LBS.ByteString deriving Show
