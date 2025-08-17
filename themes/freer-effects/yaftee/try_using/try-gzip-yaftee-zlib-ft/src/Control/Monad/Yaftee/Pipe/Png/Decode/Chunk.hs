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

	readBytes, Sequence, Crc32,

	isChunkName

	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.MonoTraversable.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Foldable
import Data.Bool
import Data.Char
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree qualified as Seq
import Data.Word.Word8 qualified as Seq

chunkRun_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates nm `Append` es) i o r ->
	Eff.E es i o ()
chunkRun_ = void
	. (`State.runN` Crc32 Crc32.initial)
	. (`State.runN` Sequence BSF.empty)
	. (`State.runN` Chunk False (seqFromString "IHDR"))

type ChunkStates nm =
	'[State.Named nm Chunk, State.Named nm Sequence, State.Named nm Crc32]

type ChunkMembers nm es = (
	U.Member (State.Named nm Chunk) es,
	U.Member (State.Named nm Sequence) es,
	U.Member (State.Named nm Crc32) es )

chunk :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (State.Named nm Sequence) es,
	U.Member (Except.E String) es
	) =>
	Int -> Eff.E es BSF.ByteString BSF.ByteString ()
chunk nm = doWhile_ . chunk1 nm

chunk1 :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	ChunkMembers nm es,
	U.Member (Except.E String) es
	) =>
	Int -> Eff.E es BSF.ByteString BSF.ByteString Bool
chunk1 nm m = do
	n <- Seq.toBitsBE <$> readBytes nm 4
	resetCrc32 nm
	cn <- readBytes nm 4
	State.putN nm $ Chunk True cn
	Pipe.yield ""
	State.putN nm $ Chunk False cn
--	Pipe.yield . Left $ ChunkBegin cn
--	for_ (split m n) \n' -> Pipe.yield =<< Right <$> readBytes nm n'
	for_ (split m n) \n' -> Pipe.yield =<< readBytes nm n'
--	Pipe.yield =<< readBytes nm n
	compCrc32 nm
	Crc32 crc1 <- State.getN nm
	crc0 <- Crc32.fromWord . Seq.toBitsBE <$> readBytes nm 4
	when (crc1 /= crc0) $ Except.throw @String "chunk1: CRC32 error"
--	Pipe.yield . Left $ ChunkEnd cn
	pure $ cn /= seqFromString "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m'	| n < m' -> n : go (m' - n)
			| otherwise -> [m']

seqFromString :: String -> BSF.ByteString
seqFromString = BSF.pack . (fromIntegral . ord <$>)

readBytes :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Sequence) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString o BSF.ByteString
readBytes nm n = State.getsN nm (Seq.splitAt' n . unSequence) >>= \case
	Nothing -> readMore nm
		>>= bool (Except.throw @String "no more ByteString") (readBytes nm n)
	Just (t, d) -> t <$ do
		State.modifyN nm $ Crc32 . (`PipeCrc32.step` t) . unCrc32
		State.putN nm (Sequence d)

resetCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
resetCrc32 nm = State.putN nm $ Crc32 Crc32.initial

compCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
compCrc32 nm = State.modifyN nm $ Crc32 . Crc32.complement . unCrc32

readMore :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Sequence) es ) =>
	Eff.E es BSF.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm (`appendSequence` bs)

newtype Sequence = Sequence { unSequence :: BSF.ByteString } deriving Show
newtype Crc32 = Crc32 { unCrc32 :: Crc32.C } deriving Show

appendSequence :: Sequence -> BSF.ByteString -> Sequence
appendSequence (Sequence bs1) bs2 = Sequence $ bs1 `BSF.append` bs2

data Chunk = Chunk {
	chunkBegin :: Bool,
	chunkName :: BSF.ByteString
	}
	deriving (Show, Eq)

isChunkName :: BSF.ByteString -> Chunk -> Bool
isChunkName nm0 cn = chunkName cn == nm0
