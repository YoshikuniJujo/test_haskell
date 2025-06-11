{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.Sequence.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Bits
import Data.Sequence qualified as Seq
import Data.Sequence.Word8 qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Pipe.Runlength.Compress qualified as Runlength
import Data.Gzip.Header
import Data.Deflate.Block
import Data.Word.Crc32 qualified as Crc32

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode
	ho <- openFile fpo WriteMode

	_ <- Eff.runM
		. PipeT.lengthRun @"foobar"
		. flip (State.runN @"foobar") Crc32.initial
		. flip (State.runN @"foobar") (([], []) :: Queue)
		. Runlength.run_ @"foobar" . Pipe.run
		$ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert' bsToSeq Pipe.=$= do
				Pipe.yield $ encodeGzipHeader sampleGzipHeader
				_ <- PipeCrc32.crc32' "foobar" Pipe.=$=
					PipeT.length "foobar" Pipe.=$=
					Runlength.compress "foobar" Pipe.=$=
					PipeL.bundle' 15 Pipe.=$=
					PipeT.convert'' runLengthsToBits [] Pipe.=$=
					toSequence' "foobar"
				PipeCrc32.complement "foobar"
				Pipe.yield . Seq.fromBits' =<< State.getN @Crc32.C "foobar"
				Pipe.yield . Seq.fromBits' @Word32 . fromIntegral =<< State.getN @PipeT.Length "foobar"
			Pipe.=$= PipeT.convert seqToBs Pipe.=$= PipeBS.hPutStr ho

	hClose h
	hClose ho

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

seqToBs :: Seq.Seq Word8 -> BS.ByteString
seqToBs = BS.pack . toList

type Bit = Bool

pattern O :: Bit
pattern O = False

type Queue = ([Bit], [Bit])

append :: Queue -> [Bit] -> Queue
append (xs, ys) bs = (xs, reverse bs ++ ys)

uncons :: Queue -> Maybe (Bit, Queue)
uncons = \case
	([], []) -> Nothing
	([], ys) -> uncons (reverse ys, [])
	(x : xs, ys) -> Just (x, (xs, ys))

popByte :: Queue -> Maybe (Word8, Queue)
popByte = ((listToNum `first`) <$>) . unfoldrN 8 uncons

listToNum :: Bits n => [Bit] -> n
listToNum = foldr (\b s -> bool id (`setBit` 0) b $ s `shiftL` 1) zeroBits

toSequence' :: forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Queue) es) =>
	Eff.E es [Bit] (Seq.Seq Word8) ()
toSequence' nm = fix \go -> Pipe.isMore >>= bool
	do	State.modifyN nm $ (`append` [O, O, O, O, O, O, O])
		Pipe.yield =<< uncurry (<$)
			. (Seq.fromList *** State.putN nm)
			. unfoldr' popByte =<< State.getN nm 
	do	State.modifyN nm . flip append =<< Pipe.await
		Pipe.yield =<< uncurry (<$)
			. (Seq.fromList *** State.putN nm)
			. unfoldr' popByte =<< State.getN nm
		go

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f = fix \go s -> maybe ([], s) (\(x, s') -> (x :) `first` go s') $ f s

unfoldrN :: Int -> (a -> Maybe (b, a)) -> a -> Maybe ([b], a)
unfoldrN 0 _ x = Just ([], x)
unfoldrN n f x = case f x of
	Nothing -> Nothing
	Just (y, x') -> ((y :) `first`) <$> unfoldrN (n - 1) f x'
