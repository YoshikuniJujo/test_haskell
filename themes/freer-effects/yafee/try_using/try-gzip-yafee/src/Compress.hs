{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Compress where

import Foreign.Marshal.Alloc
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.IO qualified as YIO
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Pipe.IO qualified as PipeIO
import Control.Monad.Yafee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

import System.IO

import RunLength
import Triple

import Calc

compressRL :: (
	Union.Member (State.S Triple) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs ) =>
	Eff.E (Pipe.P BS.ByteString RunLength ': effs) ()
compressRL = fix \go -> do
	mb <- get
	mb1 <- getAhead
	mb2 <- getAhead
	case mb of
		Nothing -> pure ()
		Just b -> do
			case (mb1, mb2) of
				(Just b1, Just b2) -> do
					st <- State.get
					mil <- getIndexLength st (BS.pack [b, b1, b2]) (fromJust <$> getAhead)
					case mil of 
						Nothing -> do
							State.modify (`updateTriple` b)
							Pipe.yield (RunLengthLiteral b)
						Just (i, l) -> do
							d <- State.gets $ calcDistance i
							bs <- getBytes $ 2 + l
							State.modify \st' -> foldl updateTriple st' (b : BS.unpack bs)
							Pipe.yield $ RunLengthLenDist (l + 3) d
				_ -> do	State.modify (`updateTriple` b)
					Pipe.yield (RunLengthLiteral b)
			go

tryCompress :: IO (((((), [()]), Triple), BS.ByteString), AheadPos)
tryCompress = withFile "samples/abcdef4.txt" ReadMode \h ->
	Eff.runM . (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
		. (`State.run` triple0) . Pipe.run
		$ PipeBS.hGet 100 h Pipe.=$= compressRL Pipe.=$= fix \go ->
			Pipe.await >>= maybe (pure ()) (\bs -> YIO.print bs >> go)

tryCompress' :: FilePath -> IO (((((), [()]), Triple), BS.ByteString), AheadPos)
tryCompress' fp = withFile fp WriteMode \hw -> alloca \p ->
	withFile "samples/abcdef4.txt" ReadMode \h ->
		Eff.runM . (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
			. (`State.run` triple0) . Pipe.run
			$ PipeBS.hGet 100 h Pipe.=$= compressRL Pipe.=$=
				(fix \go -> Pipe.await >>= maybe (pure ()) (\bs -> Pipe.yield (runLengthToWord32 bs) >> go)) Pipe.=$=
				listToAtom Pipe.=$=
				PipeIO.hPutStorable hw p

listToAtom :: Eff.E (Pipe.P [a] a ': effs) ()
listToAtom = fix \go -> Pipe.await >>= \case
	Nothing -> pure ()
	Just xs -> Pipe.yield `mapM_` xs >> go

get :: (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) => Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe Word8)
get = State.gets BS.uncons >>= \case
	Nothing -> bool (pure Nothing) get =<< readMore
	Just (b, bs) -> do
		State.put $ AheadPos 0
		State.put bs
		pure $ Just b

getBytes :: (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) => Int -> Eff.E (Pipe.P BS.ByteString o ': effs) BS.ByteString
getBytes n = State.get >>= \bs ->
	if BS.length bs >= n then BS.take n bs <$ State.put (BS.drop n bs) else
		readMore >> getBytes n

getAhead :: (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe Word8)
getAhead = do
	bs <- State.get
	AheadPos i <- State.get
	case bs BS.!? i of
		Nothing -> bool (pure Nothing) getAhead =<< readMore
		Just b -> Just b <$ State.modify nextAheadPos

readMore :: Union.Member (State.S BS.ByteString) effs =>
	Eff.E (Pipe.P BS.ByteString o ': effs) Bool
readMore = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BS.append` bs)

newtype AheadPos = AheadPos Int deriving Show

nextAheadPos :: AheadPos -> AheadPos
nextAheadPos (AheadPos p) = AheadPos $ p + 1

tryDecompress :: FilePath -> IO (Either String ((), [RunLength]))
tryDecompress fp = withFile fp ReadMode \h -> alloca \p ->
	Eff.runM . Except.run . Pipe.run $
		PipeIO.hGetStorable h p Pipe.=$=
		word32ToRunLength Pipe.=$=
		PipeIO.print

tryDecompress' :: FilePath -> IO (Either String (((), [RunLength]), Seq.Seq Word8))
tryDecompress' fp = withFile fp ReadMode \h -> alloca \p ->
	Eff.runM . Except.run . (`State.run` Seq.empty) . Pipe.run $
		PipeIO.hGetStorable h p Pipe.=$=
		word32ToRunLength Pipe.=$=
		runLength Pipe.=$=
		PipeIO.print
