{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Zlib.Decompress (decompress) where

import Control.Arrow
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
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

decompress :: forall nm -> (
	U.Member Pipe.P es,
	Deflate.Members nm es,
	U.Member (State.S (Word32, Word32)) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()
decompress nm = do
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
--	(Deflate.decompress nm 65 `Except.catch` IO.print @String) Pipe.=$= adler32'
	_ <- Deflate.decompress nm 65 Pipe.=$= adler32'
	State.putN nm $ OnDemand.RequestBytes 4
	cs0 <- BS.toBitsBE @Word32 <$> skipLeft1
	cs1 <- uncurry (.|.) . second (`shiftL` 16) <$> State.get @(Word32, Word32)
	when (cs0 /= cs1) $ Except.throw @String "zlib: Alder-32 checksum error"

skipLeft1 :: U.Member Pipe.P es => Eff.E es (Either a b) o b
skipLeft1 = Pipe.await >>=
	either (const $ Pipe.await >>= either (const $ error "bad") pure) pure

msgNotRight :: String
msgNotRight = "not right"

adler32Step :: (Word32, Word32) -> BS.ByteString -> (Word32, Word32)
adler32Step = BS.foldl \(a, b) w ->
	((a + fromIntegral w) `mod` 65521, (b + a + fromIntegral w) `mod` 65521)

_adler32 :: (
	U.Member Pipe.P es,
	U.Member (State.S (Word32, Word32)) es ) =>
	Eff.E es BS.ByteString BS.ByteString r
_adler32 = forever do
	bs <- Pipe.await
	State.modify (`adler32Step` bs)
	Pipe.yield bs

adler32' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Word32, Word32)) es ) =>
	Eff.E es BS.ByteString BS.ByteString ()
adler32' = Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just bs -> do
		State.modify (`adler32Step` bs)
		Pipe.yield bs
		adler32'
