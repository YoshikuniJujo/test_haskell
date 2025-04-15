{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Block (blocks, format, getRightJust) where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Foldable
import Data.Bits
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

import BitArray qualified as BitArray
import HuffmanTree
import Pipe.Huffman
import Calc
import RunLength

import Pipe.ByteString.OnDemand

import Debug.Trace

blocks :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (State.S (Seq.Seq Word8)) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs
	) =>
	Eff.E (Pipe.P (Either BitArray.B BS.ByteString) (Either Word8 BS.ByteString) ': effs) ()
blocks = fix (\go -> block' >>= bool (pure ()) go) Pipe.=$= runLength

block' :: (
	Union.Member (State.S Request) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.Named "bits" BitArray.B) effs
	) =>
	Eff.E (Pipe.P (Either BitArray.B BS.ByteString) RunLength ': effs) Bool
block' = do
	State.put $ RequestBits 1
	Just (Left (Just t)) <- (either (Left . BitArray.toWord8) Right <$>)
		<$> Pipe.await @(Either BitArray.B BS.ByteString)
	State.put $ RequestBits 2
	Just bt <- BitArray.toWord8 <$> (
		getLeftJust =<< Pipe.await @(Either BitArray.B BS.ByteString) )
	case bt of
		0 -> do	State.put $ RequestBytes 4
			ln <- getWord16FromPair =<< skipLeft1
			for_ (separate 10 ln) \ln' -> do
				State.put $ RequestBytes ln'
				Pipe.yield . RunLengthLiteralBS =<< getRightJust =<< Pipe.await
		_	| bt == 1 || bt == 2 -> do
			(mhlithdist, mhclen) <- whenDef (Nothing, Nothing) (bt == 2) do
				State.put $ RequestBits 5
				Just hlit <- ((+ 257) . fromIntegral <$>)
					. BitArray.toWord8 <$> (getLeftJust =<< Pipe.await)
				Just hdist <- ((+ 1) . fromIntegral <$>)
					. BitArray.toWord8 <$> (getLeftJust =<< Pipe.await)
				State.put $ RequestBits 4
				Just hclen <- ((+ 4) . fromIntegral <$>)
					. BitArray.toWord8 <$> (getLeftJust =<< Pipe.await)
				pure (Just (hlit, hlit + hdist), Just hclen)
			bits' Pipe.=$= bitsBlock mhclen mhlithdist
			bf <- State.getN "bits"
			trace (show bf) (pure ())
			State.putN "bits" BitArray.empty
			State.put $ RequestPushBack bf
			Just (Right "") <- Pipe.await
			pure ()
		_ -> Except.throw $ "No such BType: " ++ show bt
	pure (t /= 1)

bitsBlock :: (
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member Fail.F effs ) =>
	Maybe Int -> Maybe (Int, Int) -> Eff.E (Pipe.P BitArray.Bit RunLength ': effs) ()
bitsBlock mhclen mhlithdist = do
	whenMaybe mhclen \hclen -> (State.put . (id &&& id) =<<)
		. (mkTr @Word8 codeLengthList <$>)
		. replicateM hclen . (BitArray.bitsToNum <$>)
		$ replicateMMaybes 3 Pipe.await
	huffmanPipe Pipe.=$= do
		(lct, dct) <- whenMaybeDef
			(fixedTable, fixedDstTable) mhlithdist \(hlit, hld) ->
			(mkTr [0 ..] *** mkTr [0 ..])
			. Prelude.splitAt hlit <$> getCodeTable hld
		State.put $ (id &&& id) lct
		putDecoded lct dct 0

bits' :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E (Pipe.P (Either BitArray.B BS.ByteString) BitArray.Bit ': effs) ()
bits' = popBit >>= \case
	Nothing -> pure ()
	Just b -> Pipe.yield b >> bits'

popBit :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (Except.E String) effs
	) =>
	Eff.E (Pipe.P (Either BitArray.B BS.ByteString) o ': effs) (Maybe BitArray.Bit)
popBit = State.getsN "bits" BitArray.pop >>= \case
	Nothing -> do
		State.put $ RequestBuffer 100
		State.putN "bits"
			. either id BitArray.fromByteString =<< getJust =<< Pipe.await
		popBit
	Just (b, ba') -> Just b <$ State.putN "bits" ba'

whenDef :: Applicative m => a -> Bool -> m a -> m a
whenDef d b a = bool (pure d) a b

whenMaybe :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenMaybe mx f = case mx of Nothing -> pure (); Just x -> f x

whenMaybeDef :: Applicative m => b -> Maybe a -> (a -> m b) -> m b
whenMaybeDef d mx f = case mx of Nothing -> pure d; Just x -> f x

replicateMMaybes :: Applicative f => Int -> f (Maybe a) -> f [a]
replicateMMaybes n = (catMaybes <$>) . replicateM n

codeLengthList :: [Int]
codeLengthList =
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

putDecoded :: (
	Union.Member (Pipe.P (Either Int Word16) RunLength) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	BinTree Int -> BinTree Int -> Int -> Eff.E effs ()
putDecoded t dt pri = do
	mi <- Pipe.await' @(Either Int Word16) RunLength
	case mi of
		Just (Left 256) -> pure ()
		Just (Left i)
			| 0 <= i && i <= 255 -> do
				Pipe.yield' (Either Int Word16) (RunLengthLiteral $ fromIntegral i)
				putDecoded t dt 0
			| 257 <= i && i <= 264 -> State.put (dt, dt) >> putDist t dt (calcLength i 0) 0
			| 265 <= i && i <= 284 -> do
				State.put . ExtraBits $ (i - 261) `div` 4
				putDecoded t dt i
			| i == 285 -> State.put (dt, dt) >> putDist t dt (calcLength i 0) 0
			| otherwise -> error $ "putDecoded: yet " ++ show i
		Just (Right eb) -> do
			State.put (dt, dt)
			putDist t dt (calcLength pri eb) 0
		Nothing -> pure ()

putDist :: (
	Union.Member (Pipe.P (Either Int Word16) RunLength) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	BinTree Int -> BinTree Int -> RunLengthLength -> Int -> Eff.E effs ()
putDist t dt ln pri = do
	mi <- Pipe.await' @(Either Int Word16) RunLength
--	Pipe.print' mi
	case mi of
		Just (Left i)
			| 0 <= i && i <= 3 -> do
				Pipe.yield' (Either Int Word16) (RunLengthLenDist ln (calcDist i 0))
				State.put (t, t)
				putDecoded t dt 0
			| 4 <= i && i <= 29 -> do
				State.put . ExtraBits $ (i - 2) `div` 2
				putDist t dt ln i
			| otherwise -> error $ "putDist: yet " ++ show i
		Just (Right eb) -> do
			Pipe.yield' (Either Int Word16) (RunLengthLenDist ln (calcDist pri eb))
			State.put (t, t)
			putDecoded t dt 0
		_ -> error $ "putDist: yet"

getCodeTable :: forall o effs . (
	Union.Member (State.S ExtraBits) effs,
	Union.Member Fail.F effs
	) =>
	Int -> Eff.E (Pipe.P (Either Int Word16) o ': effs) [Int]
getCodeTable 0 = pure []
getCodeTable n = Pipe.await >>= \case
	Nothing -> pure []
	Just (Left ln)
		| 0 <= ln && ln <= 15 -> (ln :) <$> getCodeTable @o (n - 1)
		| ln == 16 -> error "yet"
		| ln == 17 -> do
			State.put $ ExtraBits 3
			Just (Right eb) <- Pipe.await
			(replicate (fromIntegral eb + 3) 0 ++) <$> getCodeTable @o (n - fromIntegral eb - 3)
		| ln == 18 -> do
			State.put $ ExtraBits 7
			Just (Right eb) <- Pipe.await
			(replicate (fromIntegral eb + 11) 0 ++) <$> getCodeTable @o (n - fromIntegral eb - 11)
		| otherwise -> error "yet"
	Just (Right _) -> error "bad"

skipLeft1 :: -- forall effs . forall o -> forall a -> forall b -> (
	Union.Member (Except.E String) effs =>
	Eff.E (Pipe.P (Either a b) o ': effs) b
skipLeft1 = Pipe.await >>= \case
	Just (Left _) -> Pipe.await >>= \case
		Just (Left _) -> Except.throw @String "Not Right"
		Just (Right x) -> pure x
		Nothing -> Except.throw @String "Not enough input"
	Just (Right x) -> pure x
	Nothing -> Except.throw @String "Not enough input"

getWord16FromPair :: (
	Union.Member (Except.E String) effs,
	Integral n
	) =>
	BS.ByteString -> Eff.E effs n
getWord16FromPair bs0 = fromIntegral @Word16 <$> do
	when (BS.length bs0 /= 4)
		$ Except.throw @String "getWord16FromPair: not 4 bytes"
	when (ln /= complement cln)
		$ Except.throw @String "bad pair"
	pure ln
	where
	(ln, cln) = (tow16 *** tow16) $ BS.splitAt 2 bs0
	tow16 bs = case BS.unpack bs of
		[b0, b1] -> fromIntegral b0 .|. (fromIntegral b1) `shiftL` 8
		_ -> error "never occur"

separate :: Int -> Int -> [Int]
separate bs ln
	| ln == 0 = [] | ln <= bs = [ln]
	| otherwise = bs : separate bs (ln - bs)

getRightJust ::
	Union.Member (Except.E String) effs =>
	Maybe (Either a b) -> Eff.E effs b
getRightJust = getRight <=< getJust

getRight :: Union.Member (Except.E String) effs => Either a b -> Eff.E effs b
getRight = \case
	Left _ -> Except.throw @String "No Right"
	Right x -> pure x

getJust :: Union.Member (Except.E String) effs => Maybe a -> Eff.E effs a
getJust = \case
	Nothing -> Except.throw @String "Not Just"
	Just x -> pure x

getLeftJust :: Union.Member (Except.E String) effs =>
	Maybe (Either a b) -> Eff.E effs a
getLeftJust = getLeft <=< getJust

getLeft :: Union.Member (Except.E String) effs => Either a b -> Eff.E effs a
getLeft = \case
	Left x -> pure x
	Right _ -> Except.throw @String "Not Left"

format :: (
	Union.Member (State.Named "format" BS.ByteString) effs
	) =>
	Int -> Eff.E (Pipe.P (Either Word8 BS.ByteString) BS.ByteString ': effs) ()
format n = do
	b <- checkLength n
	if b
	then yieldLen n >> format n
	else readMore2 >>= bool
		(Pipe.yield' (Either Word8 BS.ByteString) =<<
			State.getN @BS.ByteString "format")
		(format n)

checkLength :: (
	Union.Member (State.Named "format" BS.ByteString) effs
	) =>
	Int -> Eff.E effs Bool
checkLength n = do
	bs <- State.getN "format"
	pure $ BS.length bs >= n

yieldLen :: forall i effs . (
	Union.Member (State.Named "format" BS.ByteString) effs ) =>
	Int -> Eff.E (Pipe.P i BS.ByteString ': effs) ()
yieldLen n = do
	bs <- State.getN "format"
	let	(r, bs') = BS.splitAt n bs
	State.putN "format" bs'
	Pipe.yield' i r

readMore2 :: (
	Union.Member (State.Named "format" BS.ByteString) effs,
	Union.Member (Pipe.P (Either Word8 BS.ByteString) BS.ByteString) effs
	) =>
	Eff.E effs Bool
readMore2 = Pipe.await' BS.ByteString >>= \case
	Nothing -> pure False
	Just (Left w) -> True <$ State.modifyN "format" (`BS.snoc` w)
	Just (Right bs) -> True <$ State.modifyN "format" (`BS.append` bs)
