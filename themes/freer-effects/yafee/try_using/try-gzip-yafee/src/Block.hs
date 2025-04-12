{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Block where

import Control.Arrow
import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Foldable
import Data.Bits
import Data.Maybe
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

import Pipe.ByteString

import BitArray
import HuffmanTree
import Pipe.Huffman
import Calc
import ByteStringNum

readBlock :: forall effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (Except.E String) effs,
	Union.Member Union.Fail effs, Union.Member IO effs ) =>
	Int -> Eff.E (Pipe BS.ByteString RunLength effs) Bool
readBlock bffsz = do
	Just nf <- ((/= (1 :: Word8)) <$>) <$> takeBit8' 1
	mbt <- takeBit8 @RunLength 2
	let	bt = maybe 3 id mbt
	if bt == 0
	then readNonCompressed bffsz Pipe.=$=
		toRunLength @(Pipe BS.ByteString RunLength effs)
	else do	(mhlithdist, mhclen) <- whenDef (Nothing, Nothing) (bt == 2) do
			Just hlit <- ((+ 257) <$>) <$> takeBit8' @RunLength 5
			Just hdist <- ((+ 1) <$>) <$> takeBit8' @RunLength 5
			Just hclen <- ((+ 4) <$>) <$> takeBit8' @RunLength 4
			pure $ (Just (hlit, hlit + hdist), Just hclen)
		bits Pipe.=$= do
			whenMaybe mhclen \hclen -> (State.put . (id &&& id) =<<)
				. (mkTr @Word8 codeLengthList <$>)
				. replicateM hclen . (bitListToNum <$>)
				$ replicateMMaybes 3 Pipe.await
			huffmanPipe Pipe.=$= do
				(lct, dct) <-
					whenMaybeDef (fixedTable, fixedDstTable)
						mhlithdist \(hlit, hld) ->
						(mkTr [0 ..] *** mkTr [0 ..])
							. Prelude.splitAt hlit
							<$> getCodeTable hld
				State.put $ (id &&& id) lct
				putDecoded lct dct 0
	pure nf

data RunLength = RunLengthLiteral Word8 | RunLengthLenDist RunLengthLength RunLengthDist deriving Show

data RunLengthLength = RunLengthLength Int deriving Show

data RunLengthDist = RunLengthDist Int deriving Show

runLengthLength :: Int -> Word16 -> RunLengthLength
runLengthLength i eb = RunLengthLength $ calcLength i eb

runLengthDist :: Int -> Word16 -> RunLengthDist
runLengthDist i eb = RunLengthDist $ calcDist i eb

type Pipe i o effs = (Pipe.P i o ': effs)

toRunLength :: (
	Union.Member (Pipe.P BS.ByteString RunLength) effs
	) =>
	Eff.E effs ()
toRunLength = Pipe.await' RunLength >>= \case
	Nothing -> pure ()
	Just bs -> for_ (BS.unpack bs) $ Pipe.yield' BS.ByteString . RunLengthLiteral

readNonCompressed :: forall effs . (
	Union.Member (Pipe.P BS.ByteString BS.ByteString) effs,
	Union.Member (State.S BitArray) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,
	Union.Member IO effs ) =>
	Int -> Eff.E effs ()
readNonCompressed bffsz = do
	Pipe.print' =<< takeByteBoundary @BS.ByteString
	ln <- takeWord16FromPair
	forM_ (separate bffsz ln) \ln' -> do
		bs <- takeBytes @BS.ByteString ln'
		maybe (pure ()) (Pipe.yield' BS.ByteString) bs
	where
	takeWord16FromPair = do
		Just ln <- (bsToNum <$>) <$> takeBytes @BS.ByteString 2
		Just nln <- (bsToNum <$>) <$> takeBytes @BS.ByteString 2
		when ((ln `xor` complement nln) .&. 0xffff /= 0)
			$ Except.throw @String "bad boo"
		pure ln
	separate bs ln
		| ln == 0 = [] | ln <= bs = [ln]
		| otherwise = bs : separate bs (ln - bs)

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
			| 257 <= i && i <= 264 -> State.put (dt, dt) >> putDist t dt (runLengthLength i 0) 0
			| 265 <= i && i <= 284 -> do
				State.put . ExtraBits $ (i - 261) `div` 4
				putDecoded t dt i
			| i == 285 -> State.put (dt, dt) >> putDist t dt (runLengthLength i 0) 0
			| otherwise -> error $ "putDecoded: yet " ++ show i
		Just (Right eb) -> do
			State.put (dt, dt)
			putDist t dt (runLengthLength pri eb) 0
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
				Pipe.yield' (Either Int Word16) (RunLengthLenDist ln (runLengthDist i 0))
				State.put (t, t)
				putDecoded t dt 0
			| 4 <= i && i <= 29 -> do
				State.put . ExtraBits $ (i - 2) `div` 2
				putDist t dt ln i
			| otherwise -> error $ "putDist: yet " ++ show i
		Just (Right eb) -> do
			Pipe.yield' (Either Int Word16) (RunLengthLenDist ln (runLengthDist pri eb))
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
