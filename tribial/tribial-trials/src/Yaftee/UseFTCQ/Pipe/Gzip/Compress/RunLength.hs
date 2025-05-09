{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.RunLength (
	compressRL
	) where

import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.OpenUnion qualified as Union
import Data.Maybe
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Pipe.Gzip.RunLength (RunLength)
import Yaftee.UseFTCQ.Pipe.Gzip.RunLength qualified as RunLength
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Triple
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.AheadPos

compressRL :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Triple) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs ) =>
	Eff.E effs BS.ByteString RunLength ()
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
					mil <- getIndexLength st (BS.pack [b, b1, b2]) getAhead
					case mil of 
						Nothing -> do
							State.modify (`updateTriple` b)
							Pipe.yield (RunLength.Literal b)
						Just (i, l) -> do
							d <- State.gets $ calcDistance i
							let	c1 = (l, (l + 1, [RunLength.LenDist (l + 3) d]))
							State.modify (`updateTriple` b)
							b' <- fromJust <$> get
							_mb2' <- getAhead
							mb3 <- getAhead

							case mb3 of
								Nothing -> do
									State.modify (`updateTriple` b')
									runCandidate c1
								Just b3 -> do
									st' <- State.get
									mil' <- getIndexLength st' (BS.pack [b', b2, b3]) getAhead
									case mil' of
										Nothing -> do
											State.modify (`updateTriple` b')
											runCandidate c1
										Just (i', l') -> do
											d' <- State.gets $ calcDistance i'
											State.modify (`updateTriple` b')
											let	c2 = (l', (2 + l', [
													RunLength.Literal b,
													-- RunLength.Literal 0x39,
													RunLength.LenDist (l' + 3) d']))
											if l + 1 >= l'
											then runCandidate c1
											else runCandidate c2
				_ -> do	State.modify (`updateTriple` b)
					Pipe.yield (RunLength.Literal b)
			go

runCandidate :: (
	Foldable t,
	Union.Member Pipe.P effs,
	Union.Member (State.S AheadPos) effs,
	Union.Member (State.S Triple) effs,
	Union.Member (State.S BS.ByteString) effs
	) =>
	(a, (Int, t o)) -> Eff.E effs BS.ByteString o ()
runCandidate (_l, (l', ys)) = do
	bs <- getBytes l'
	State.modify \st' -> foldl updateTriple st' $ BS.unpack bs
	Pipe.yield `mapM_` ys

newtype FileLength = FileLength Int deriving Show

get :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) => Eff.E effs BS.ByteString o (Maybe Word8)
get = State.gets BS.uncons >>= \case
	Nothing -> bool (pure Nothing) get =<< readMore
	Just (b, bs) -> do
		State.put $ AheadPos 0
		State.put bs
		pure $ Just b

getBytes :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) => Int -> Eff.E effs BS.ByteString o BS.ByteString
getBytes n = State.get >>= \bs ->
	if BS.length bs >= n then BS.take n bs <$ State.put (BS.drop n bs) else
		readMore >> getBytes n

getAhead :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S AheadPos) effs
	) =>
	Eff.E effs BS.ByteString o (Maybe Word8)
getAhead = do
	bs <- State.get
	AheadPos i <- State.get
	case bs BS.!? i of
		Nothing -> bool (pure Nothing) getAhead =<< readMore
		Just b -> Just b <$ State.modify nextAheadPos

readMore :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BS.ByteString) effs ) =>
	Eff.E effs BS.ByteString o Bool
readMore = Pipe.isMore >>= bool (pure False)
	(True <$ (State.modify . (flip BS.append) =<< Pipe.await))
