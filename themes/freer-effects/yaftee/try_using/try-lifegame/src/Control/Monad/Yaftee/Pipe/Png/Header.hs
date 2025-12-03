{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Header (

	-- * DECODE HEADER

	runHeader', StatesHeader', decodeHeader'

	) where

import Prelude hiding (Monoid, read)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as ChunkNew
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header.Data qualified as Header

import Data.Word.Crc32 qualified as Crc32
import Data.Png.Header.Data
import Data.Word.Word8 qualified as BSF

data ColorType = Rgb | Rgba deriving Show
data BitDepth = BitDepth8 | BitDepth16 deriving Show

runHeader' :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (StatesHeader' nm `Append` es) i o r -> Eff.E es i o Header.Header
runHeader' = (snd <$>)
	. flip (State.runN @nm) Header.header0
	. OnDemand.run @nm

type StatesHeader' nm =
	OnDemand.States nm `Append` '[State.Named nm Header.Header]

decodeHeader' :: forall nm nm' -> (
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es,
	U.Member Pipe.P es,
	MembersHeader' nm' es,
	U.Member (Except.E String) es ,
	U.Member Fail.F es
	) =>
	Eff.E es BSF.ByteString o ()
decodeHeader' nm nm' = void $
	ChunkNew.decode nm 500
	Pipe.=$= forever do
		ChunkNew.Begin _ cnm <- Pipe.await
		ChunkNew.Body x <- Pipe.await
		ChunkNew.End <- Pipe.await
		when (cnm == "IHDR" || cnm == "IDAT") $ Pipe.yield x
	Pipe.=$= OnDemand.onDemand nm' Pipe.=$= read nm' (const $ pure ())

type MembersHeader' nm es = (
	OnDemand.Members nm es,
	U.Member (State.Named nm Header.Header) es )

read :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Header) es ) =>
	(Header -> Eff.E es BSF.ByteString o ()) ->
		Eff.E es BSF.ByteString o ()
read nm proc = do
	State.putN nm $ OnDemand.RequestBytes 4
	w <- BSF.toBitsBE <$> Pipe.await
	h <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	bd <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	ct <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	fm <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	im <- BSF.toBitsBE <$> Pipe.await
	let	hdr = Header {
			headerWidth = w, headerHeight = h,
			headerBitDepth = bd,
			headerColorType = ct,
			headerCompressionMethod = cm,
			headerFilterMethod = fm,
			headerInterlaceMethod = im }
	proc hdr
	State.putN nm hdr
