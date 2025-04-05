{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitArray where

import Prelude hiding (splitAt)

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

data Bit = O | I deriving (Show, Eq, Ord)

runBitArray :: BS.ByteString ->
	Eff.E (State.S BS.ByteString ': State.S BitInfo ': effs) a ->
	Eff.E effs ((a, BS.ByteString), BitInfo)
runBitArray bs = (`State.run` BitInfo 0 (BS.length bs * 8)) . (`State.run` bs)

data BitInfo = BitInfo { bit0 :: Int, bitsLen :: Int } deriving Show

data BitArray = BitArray { bitInfo :: BitInfo, bitsBody :: BS.ByteString }
	deriving Show

pop :: forall o effs . (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs
	) =>
	Eff.E effs (Maybe Bit)
pop = do
	BitInfo { bit0 = i, bitsLen = ln } <- State.get
	case (i, ln) of
		(_, 0) -> do
			b <- readMore @o
			if b then pop @o else pure Nothing
		(7, _) -> State.gets BS.uncons >>= \case
			Just (b, bs) -> do
				State.put bs
				Just (bool O I (testBit b 7))
					<$ State.put BitInfo { bit0 = 0, bitsLen = ln - 1 }
			Nothing -> error "bad"
		_ -> do	b <- (`testBit` i) . head' (show ln) <$> State.get
			Just (bool O I b) <$ State.put BitInfo { bit0 = i + 1, bitsLen = ln - 1 }

head' :: String -> BS.ByteString -> Word8
head' i bs = if BS.null bs then error ("head': bad " ++ i) else BS.head bs

takeBitArray :: forall o effs . (
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (Pipe.P BS.ByteString o) effs
	) =>
	Int -> Eff.E effs (Maybe BitArray)
takeBitArray n = do
	info <- State.get
	bs <- State.get
	case splitAt n $ BitArray info bs of
		Nothing -> do
			b <- readMore @o
			if b then takeBitArray @o n else pure Nothing
		Just (t, BitArray info' bs') -> Just t <$ (State.put info' >> State.put bs')
{-
	BitInfo { bit0 = i, bitsLen = ln } <- get
	if ln < n
	then do	b <- readMore
		if b then takeBitArray n else pure Nothing
	else do	info <- get
		bs <- get

		State.put BitInfo { bit0 = (i + n) `mod` 8, bitsLen = ln - n }
		BitArray (BitInfo i n) (BS.take 
		-}

splitAt :: Int -> BitArray -> Maybe (BitArray, BitArray)
splitAt n (BitArray (BitInfo i ln) bs)
	| ln < n = Nothing
	| otherwise = Just (
		normalize $ BitArray (BitInfo i n) bs,
		normalize $ BitArray (BitInfo (i + n) (ln - n)) bs )

readMore :: forall o effs . (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) =>
	Eff.E effs Bool
readMore = Pipe.await @_ @o >>= \case
	Nothing -> pure False
	Just bs -> True <$ do
		State.modify (`BS.append` bs)
		State.modify \(BitInfo i ln) -> BitInfo i (ln + BS.length bs * 8)

normalize :: BitArray -> BitArray
normalize (BitArray (BitInfo i ln) bs)
	| 0 <= i = BitArray (BitInfo i' ln)
		. BS.take t $ BS.drop (i `div` 8) bs
	| otherwise = error "bad"
	where
	i' = i `mod` 8
	t = (ln + i' - 1) `div` 8 + 1

bitArrayToWord8 :: BitArray -> Maybe Word8
bitArrayToWord8 (BitArray (BitInfo i ln) bs)
	| ln + i <= 8 = Just $
		BS.head bs `shiftR` i .&. foldl setBit zeroBits [0 .. ln - 1]
	| ln <= 8 = Just let b0 = BS.head bs; b1 = BS.head $ BS.tail bs in
		b0 `shiftR` i .|.
		b1 `shiftL` (8 - i) .&. foldl setBit zeroBits [0 .. ln - 1]
	| otherwise = Nothing

takeBit8 :: forall o effs . (
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (Pipe.P BS.ByteString o) effs
	) =>
	Int -> Eff.E effs (Maybe Word8)
takeBit8 n = (bitArrayToWord8 =<<) <$> takeBitArray @o n

bits :: (
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (Pipe.P BS.ByteString Bit) effs
	) =>
	Eff.E effs ()
bits = do
	mb <- pop @Bit
	case mb of
		Nothing -> pure ()
		Just b -> Pipe.yield @BS.ByteString b >> bits
