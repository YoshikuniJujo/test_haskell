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
	Eff.E (State.S BitArray ': effs) a -> Eff.E effs (a, BitArray)
runBitArray bs = (`State.run` BitArray 0 (BS.length bs * 8) bs)

data BitInfo = BitInfo { bit0' :: Int, bitsLen' :: Int } deriving Show

data BitArray = BitArray { bit0 :: Int, bitsLen :: Int, bitsBody :: BS.ByteString }
	deriving Show

pop :: forall o effs . (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BitArray) effs ) =>
	Eff.E effs (Maybe Bit)
pop = do
	BitArray { bit0 = i, bitsLen = ln, bitsBody = bs } <- State.get
	case (i, ln) of
		(_, 0) -> do
			b <- readMore @o
			if b then pop @o else pure Nothing
		(7, _) -> State.get >>= \(BitArray _ _ bs1) -> case BS.uncons bs1 of
			Just (b, bs2) -> do
				Just (bool O I (testBit b 7))
					<$ State.put BitArray { bit0 = 0, bitsLen = ln - 1, bitsBody = bs2 }
			Nothing -> error "bad"
		_ -> do	b <- (`testBit` i) . head' (show ln) <$> State.gets bitsBody
			Just (bool O I b) <$ State.put BitArray { bit0 = i + 1, bitsLen = ln - 1, bitsBody = bs }

head' :: String -> BS.ByteString -> Word8
head' i bs = if BS.null bs then error ("head': bad " ++ i) else BS.head bs

takeBitArray :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs
	) =>
	Int -> Eff.E effs (Maybe BitArray)
takeBitArray n = do
	BitArray i0 ln bs <- State.get
	case splitAt n $ BitArray i0 ln bs of
		Nothing -> do
			b <- readMore @o
			if b then takeBitArray @o n else pure Nothing
		Just (t, BitArray i0' ln' bs') -> Just t <$ State.put (BitArray i0' ln' bs')

takeBitArray' :: forall o effs . (
	Union.Member (State.S BitArray) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe BitArray)
takeBitArray' n = do
	BitArray i0 ln bs <- State.get
	case splitAt n $ BitArray i0 ln bs of
		Nothing -> do
			b <- readMore'
			if b then takeBitArray @o n else pure Nothing
		Just (t, BitArray i0' ln' bs') -> Just t <$ State.put (BitArray i0' ln' bs')

splitAt :: Int -> BitArray -> Maybe (BitArray, BitArray)
splitAt n (BitArray i ln bs)
	| ln < n = Nothing
	| otherwise = Just (
		normalize $ BitArray i n bs,
		normalize $ BitArray (i + n) (ln - n) bs )

readMore :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) =>
	Eff.E effs Bool
readMore = Pipe.await' o >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`bitArrayAppendByteString` bs)

bitArrayAppendByteString :: BitArray -> BS.ByteString -> BitArray
bitArrayAppendByteString
	BitArray { bit0 = i0, bitsLen = ln, bitsBody = bs } bs' =
	BitArray {
		bit0 = i0, bitsLen = ln + 8 * BS.length bs',
		bitsBody = bs `BS.append` bs' }

readMore' :: forall o effs . (
	Union.Member (State.S BitArray) effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) Bool
readMore' = Pipe.await' o >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`bitArrayAppendByteString` bs)

normalize :: BitArray -> BitArray
normalize (BitArray i ln bs)
	| 0 <= i = BitArray i' ln
		. BS.take t $ BS.drop (i `div` 8) bs
	| otherwise = error "bad"
	where
	i' = i `mod` 8
	t = (ln + i' - 1) `div` 8 + 1

bitArrayToWord8 :: BitArray -> Maybe Word8
bitArrayToWord8 (BitArray i ln bs)
	| ln + i <= 8 = Just $
		BS.head bs `shiftR` i .&. foldl setBit zeroBits [0 .. ln - 1]
	| ln <= 8 = Just let b0 = BS.head bs; b1 = BS.head $ BS.tail bs in
		b0 `shiftR` i .|.
		b1 `shiftL` (8 - i) .&. foldl setBit zeroBits [0 .. ln - 1]
	| otherwise = Nothing

takeBit8 :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs
	) =>
	Int -> Eff.E effs (Maybe Word8)
takeBit8 n = (bitArrayToWord8 =<<) <$> takeBitArray @o n

takeBit8' :: forall o effs n . (
	Union.Member (State.S BitArray) effs,
	Integral n ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe n)
takeBit8' n = ((fromIntegral <$>) . bitArrayToWord8 =<<) <$> takeBitArray' n

bits :: (
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString Bit) effs
	) =>
	Eff.E effs ()
bits = do
	mb <- pop @Bit
	case mb of
		Nothing -> pure ()
		Just b -> Pipe.yield' BS.ByteString b >> bits

splitAtByteBoundary :: BitArray -> Maybe (BitArray, BitArray)
splitAtByteBoundary bs@BitArray { bit0 = i } =
	splitAt (8 - ((i - 1) `mod` 8 + 1)) bs

takeByteBoundary :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs
	) =>
	Eff.E effs (Maybe BitArray)
takeByteBoundary = do
	BitArray { bit0 = i } <- State.get
	takeBitArray @o (8 - ((i - 1) `mod` 8 + 1))


bitListToNum :: (Num n, Bits n) => [Bit] -> n
bitListToNum = foldr (\b s -> (case b of O -> 0; I -> 1) .|. s `shiftL` 1) 0
