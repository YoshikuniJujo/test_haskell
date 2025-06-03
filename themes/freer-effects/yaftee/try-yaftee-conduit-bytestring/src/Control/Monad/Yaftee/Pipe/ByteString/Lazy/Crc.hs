{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc (

	-- * PIPE AND STATE

	runCrc32, crc32, crc32', resetCrc32, compCrc32,

	-- * NO PIPE

	crc32StepBS, crc32StepBS', initialCrc32, complementCrc32,

	-- * TYPE

	Crc32(..),

	crc32ToByteString, crc32ToByteStringBE,
	crc32FromByteString, crc32FromByteStringBE

) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.Crc.Common
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Word
import Data.ByteString.Lazy qualified as LBS

crc32 :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es LBS.ByteString LBS.ByteString r
crc32 nm = State.putN nm (Crc32 0xffffffff) >> crc32Body nm

crc32Body :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es LBS.ByteString LBS.ByteString r
crc32Body nm = fix \go -> Pipe.await >>= \bs -> do
	State.modifyN nm \(Crc32 c) -> Crc32 $ c `crc32StepBS` bs
	Pipe.yield bs
	go

crc32' :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es LBS.ByteString LBS.ByteString ()
crc32' nm = State.putN nm (Crc32 0xffffffff) >> crc32Body' nm

crc32Body' :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32) es) =>
	Eff.E es LBS.ByteString LBS.ByteString ()
crc32Body' nm = fix \go -> Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just bs -> do
		State.modifyN nm \(Crc32 c) -> Crc32 $! c `crc32StepBS` bs
		Pipe.yield bs
		go

crc32StepBS :: Word32 -> LBS.ByteString -> Word32
crc32StepBS = LBS.foldl' crc32Step

crc32StepBS' :: Crc32 -> LBS.ByteString -> Crc32
crc32StepBS' = (Crc32 .) . LBS.foldl' crc32Step . unCrc32

crc32ToByteString :: Crc32 -> LBS.ByteString
crc32ToByteString (Crc32 c) =
	bs `LBS.append` LBS.replicate (4 - LBS.length bs) 0
	where
	bs = numToBs c
	numToBs 0 = ""
	numToBs n = fromIntegral (n .&. 0xff) `LBS.cons` numToBs (n `shiftR` 8)

crc32ToByteStringBE :: Crc32 -> LBS.ByteString
crc32ToByteStringBE (Crc32 c) =
	LBS.replicate (4 - LBS.length bs) 0 `LBS.append` bs
	where
	bs = numToBs c ""
	numToBs 0 b = b
	numToBs n b =
		numToBs (n `shiftR` 8) (fromIntegral (n .&. 0xff) `LBS.cons` b)

crc32FromByteString :: LBS.ByteString -> Maybe Crc32
crc32FromByteString = (Crc32 <$>) . go (4 :: Int) . LBS.unpack
	where
	go 0 [] = Just 0
	go n (w : ws)
		| n > 0 = (fromIntegral w .|.) . (`shiftL` 8) <$> go (n - 1) ws
	go _ _ = Nothing

crc32FromByteStringBE :: LBS.ByteString -> Maybe Crc32
crc32FromByteStringBE = (Crc32 <$>) . go (4 :: Int) 0 . LBS.unpack
	where
	go 0 s [] = Just s
	go n s (w : ws)
		| n > 0 = go (n - 1) (fromIntegral w .|. s `shiftL` 8) ws
	go _ _ _ = Nothing
