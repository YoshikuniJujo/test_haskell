{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.C.ByteArray qualified as CByteArray
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import System.IO
import System.Environment

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core
import Codec.Compression.Zlib.Basic.Core
import Codec.Compression.Zlib.Advanced.Core

import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.CString qualified as BSF

import Debug.Trace

inputBufSize, outputBufSize :: Int
inputBufSize = 64 * 4
outputBufSize = 64

readBufSize :: Int
readBufSize = 64 * 5

main :: IO ()
main = do
	fpi : fpo : _ <- getArgs
	(hi, ho) <- (,) <$> openFile fpi ReadMode <*> openFile fpo WriteMode
	(i, o) <- (,)
		<$> CByteArray.malloc inputBufSize
		<*> CByteArray.malloc outputBufSize

	void . Eff.runM . Except.run @ReturnCode . inflateRun @"foobar" . Pipe.run
		. (`Except.catch` IO.print @ReturnCode) . void
		$ PipeBS.hGet readBufSize hi Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			inflatePipe "foobar" IO (WindowBitsZlibAndGzip 15) i o Pipe.=$=
			PipeT.convert BSF.toStrict Pipe.=$=
			PipeBS.hPutStr ho

	CByteArray.free i; CByteArray.free o
	hClose ho; hClose hi

inflateRun :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm (Maybe BSF.ByteString) ': es) i o r ->
	Eff.E es i o (r, Maybe BSF.ByteString)
inflateRun = (`State.runN` (Nothing :: Maybe BSF.ByteString))

inflatePipe :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es, U.Member (State.Named nm (Maybe BSF.ByteString)) es,
	U.Member (Except.E ReturnCode) es,
	U.Base (U.FromFirst m) es ) =>
	WindowBits -> CByteArray.B -> CByteArray.B ->
	Eff.E es BSF.ByteString BSF.ByteString ()
inflatePipe nm m wbs (i, ni) (o, no) = do
	bs <- Pipe.await
	((i', n), ebs) <- Eff.effBase . unsafeIOToPrim @m $ BSF.poke (castPtr i, ni) bs
	either  (const $ pure ()) (putInput nm) ebs
	strm <- Eff.effBase $ streamThaw @m streamInitial {
		streamNextIn = castPtr i', streamAvailIn = fromIntegral n,
		streamNextOut = o, streamAvailOut = fromIntegral no }
	rc <- Eff.effBase (inflateInit2 @m strm wbs)
	when (rc /= Ok) $ Except.throw rc

	doWhile_ do
		rc <- Eff.effBase (inflate @m strm NoFlush)
		ai <- Eff.effBase @m $ availIn strm
		ao <- Eff.effBase @m $ availOut strm
		Pipe.yield =<< Eff.effBase (unsafeIOToPrim @m
			$ BSF.peek (castPtr o, no - fromIntegral ao))
		when (rc `notElem` [Ok, StreamEnd]) $ Except.throw rc
		when (rc /= StreamEnd && ai == 0) do
			bs' <- getInput nm
			((i'', n'), ebs') <- Eff.effBase . unsafeIOToPrim @m $ BSF.poke (castPtr i, ni) bs'
			either (const $ pure ()) (putInput nm) ebs'
			Eff.effBase $ nextIn @m strm (castPtr i'') (fromIntegral n')
		Eff.effBase . nextOut @m strm o $ fromIntegral no
		pure $ rc /= StreamEnd

	rc <- Eff.effBase (inflateEnd @m strm)
	when (rc /= Ok) $ Except.throw rc

getInput :: forall es i o . forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm (Maybe i)) es) =>
	Eff.E es i o i
getInput nm =
	State.getN nm >>= maybe Pipe.await (<$ State.putN @(Maybe i) nm Nothing)

putInput :: forall nm ->
	(U.Member (State.Named nm (Maybe i)) es) => i -> Eff.E es i o ()
putInput nm = State.putN nm . Just
