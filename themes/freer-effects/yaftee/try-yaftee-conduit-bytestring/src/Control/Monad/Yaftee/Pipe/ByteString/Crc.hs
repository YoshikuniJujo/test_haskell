{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Crc (

	-- * PIPE

	runCrc32, crc32, crc32', resetCrc32, compCrc32,

	-- * NO PIPE

	crc32StepBS, crc32StepBS', initialCrc32, complementCrc32,

	-- * TYPE

	Crc32(..),

	crc32ToByteString, byteStringToCrc32,
	crc32ToByteStringBE, byteStringToCrc32BE,

	) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.Crc.Common
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

crc32 :: forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es BS.ByteString BS.ByteString r
crc32 nm = do
	State.putN nm $ Crc32 0xffffffff
	crc32Body nm

crc32' :: forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es BS.ByteString BS.ByteString ()
crc32' nm = do
	State.putN nm $ Crc32 0xffffffff
	crc32Body' nm

crc32Body :: forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es BS.ByteString BS.ByteString r
crc32Body nm = fix \go -> Pipe.await >>= \bs -> do
	State.modifyN nm \(Crc32 c) -> Crc32 $ c `crc32StepBS` bs
	Pipe.yield bs
	go

crc32Body' :: forall es . forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es BS.ByteString BS.ByteString ()
crc32Body' nm = fix \go ->
	Pipe.isMore >>= bool (pure ()) (Pipe.await >>= \bs -> do
		State.modifyN nm \(Crc32 c) -> Crc32 $! c `crc32StepBS` bs
		Pipe.yield bs
		go)

crc32ToByteString :: Crc32 -> BS.ByteString
crc32ToByteString (Crc32 c) = bs `BS.append` BS.replicate (4 - BS.length bs) 0
	where
	bs = numToBs c
	numToBs 0 = ""
	numToBs n = fromIntegral (n .&. 0xff) `BS.cons` numToBs (n `shiftR` 8)

crc32ToByteStringBE :: Crc32 -> BS.ByteString
crc32ToByteStringBE (Crc32 c) = BS.replicate (4 - BS.length bs) 0 `BS.append` bs
	where
	bs = numToBs c ""
	numToBs 0 b = b
	numToBs n b = numToBs (n `shiftR` 8) (fromIntegral (n .&. 0xff) `BS.cons` b)

byteStringToCrc32 :: BS.ByteString -> Maybe Crc32
byteStringToCrc32 = (Crc32 <$>) . go (4 :: Int) . BS.unpack
	where
	go 0 [] = Just 0
	go n (w : ws)
		| n > 0 = (fromIntegral w .|.) . (`shiftL` 8) <$> go (n - 1) ws
	go _ _ = Nothing

byteStringToCrc32BE :: BS.ByteString -> Maybe Crc32
byteStringToCrc32BE = (Crc32 <$>) . go (4 :: Int) 0 . BS.unpack
	where
	go 0 s [] = Just s
	go n s (w : ws)
		| n > 0 =  go (n - 1) (fromIntegral w .|. s `shiftL` 8) ws
	go _ _ _ = Nothing

crc32StepBS :: Word32 -> BS.ByteString -> Word32
crc32StepBS = BS.foldl' crc32Step

crc32StepBS' :: Crc32 -> BS.ByteString -> Crc32
crc32StepBS' = (Crc32 .) . BS.foldl' crc32Step . unCrc32
