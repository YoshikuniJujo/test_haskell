{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Zlib.Decompress (

	run_, States,
	decompress, decompress', Members

	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Adler32 qualified as Adler32
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.BitArray qualified as BitArray

import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor

run_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o r -> Eff.E es i o ()
run_ = Adler32.run_ . Deflate.run_

type States nm = Deflate.States nm `Append` '[
	State.Named nm Adler32.A
	]

decompress :: forall nm -> (
	U.Member Pipe.P es, Members nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Int ->
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()
decompress nm fnum = do
	State.putN nm $ OnDemand.RequestBytes 1
	h1 <- BS.toBits @Word8 <$> (Except.getRight @String msgNotRight =<< Pipe.await)
	let	cm = h1 .&. 0xf
		cinfo = h1 `shiftR` 4
		_wnsize = (2 :: Int) ^ (cinfo + 8)
	when (cm /= 8) $ Except.throw "Compression method must be 8"
	h2 <- BS.toBits @Word8 <$> (Except.getRight @String msgNotRight =<< Pipe.await)
	let	_flevel = h2 `shiftR` 6
		fdict = (h2 `shiftR` 5) .&. 1
		chk = (fromIntegral h1 `shiftL` 8) .|. fromIntegral h2
	when (fdict /= 0) $ Except.throw "Preset dictionary must not be used"
	when ((chk :: Word16) `mod` 31 /= 0) $ Except.throw "zlib header check bits error"
--	(Deflate.decompress nm 65 `Except.catch` IO.print @String) Pipe.=$= Adler32.adler32'
	_ <- Deflate.decompress nm fnum Pipe.=$= Adler32.adler32' nm
	State.putN nm $ OnDemand.RequestBytes 4
	cs0 <- BS.toBitsBE @Word32 <$> skipLeft1
	cs1 <- Adler32.toWord32 <$> State.getN @Adler32.A nm
	when (cs0 /= cs1) $ Except.throw @String ("zlib: Alder-32 checksum error: " ++ show cs0 ++ " " ++ show cs1)

decompress' :: forall nm -> (
	U.Member Pipe.P es, Members nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Int -> Int -> Int ->
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()
decompress' nm w h bpp = do
	State.putN nm $ OnDemand.RequestBytes 1
	h1 <- BS.toBits @Word8 <$> (Except.getRight @String msgNotRight =<< Pipe.await)
	let	cm = h1 .&. 0xf
		cinfo = h1 `shiftR` 4
		_wnsize = (2 :: Int) ^ (cinfo + 8)
	when (cm /= 8) $ Except.throw "Compression method must be 8"
	h2 <- BS.toBits @Word8 <$> (Except.getRight @String msgNotRight =<< Pipe.await)
	let	_flevel = h2 `shiftR` 6
		fdict = (h2 `shiftR` 5) .&. 1
		chk = (fromIntegral h1 `shiftL` 8) .|. fromIntegral h2
	when (fdict /= 0) $ Except.throw "Preset dictionary must not be used"
	when ((chk :: Word16) `mod` 31 /= 0) $ Except.throw "zlib header check bits error"
--	(Deflate.decompress' nm 65 `Except.catch` IO.print @String) Pipe.=$= Adler32.adler32'
	_ <- Deflate.decompress' nm w h bpp Pipe.=$= Adler32.adler32' nm
	State.putN nm $ OnDemand.RequestBytes 4
	cs0 <- BS.toBitsBE @Word32 <$> skipLeft1
	cs1 <- Adler32.toWord32 <$> State.getN @Adler32.A nm
	when (cs0 /= cs1) $ Except.throw @String ("zlib: Alder-32 checksum error: " ++ show cs0 ++ " " ++ show cs1)

type Members nm es = (
	Deflate.Members nm es,
	U.Member (State.Named nm Adler32.A) es )

skipLeft1 :: U.Member Pipe.P es => Eff.E es (Either a b) o b
skipLeft1 = Pipe.await >>=
	either (const $ Pipe.await >>= either (const $ error "bad") pure) pure

msgNotRight :: String
msgNotRight = "not right"
