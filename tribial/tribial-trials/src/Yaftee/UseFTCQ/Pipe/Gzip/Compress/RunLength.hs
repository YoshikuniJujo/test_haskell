{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.RunLength (
	run, compressRL, AheadPos ) where

import Control.Monad.Fix
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.Gzip.RunLength qualified as RL
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Triple qualified as Triple
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.HFunctor qualified as HFunctor
import Yaftee.OpenUnion qualified as U

run :: HFunctor.HFunctor (U.U es) =>
	Eff.E (State.S Triple.T ': State.S AheadPos ': es) i o a ->
	Eff.E es i o ((a, Triple.T), AheadPos)
run = (`State.run` AheadPos 0) . (`State.run` Triple.empty)

compressRL :: (
	U.Member Pipe.P es,
	U.Member (State.S AheadPos) es, U.Member (State.S Triple.T) es,
	U.Member (State.S BS.ByteString) es ) => Eff.E es BS.ByteString RL.R ()
compressRL = fix \go -> get3 >>= \(mb, mb1, mb2) ->
	($ mb) $ maybe (pure ()) \b -> (>> go) case (mb1, mb2) of
		(Just b1, Just b2) -> State.get >>= \st ->
			Triple.indexLength st b b1 b2 getAhead >>= \case
				Nothing -> do
					State.modify (`Triple.update` b)
					Pipe.yield (RL.Literal b)
				Just (i, l) -> putLenDist b b1 b2 i l
		_ -> do	State.modify (`Triple.update` b)
			Pipe.yield (RL.Literal b)

putLenDist :: (
	U.Member Pipe.P es, U.Member (State.S Triple.T) es,
	U.Member (State.S AheadPos) es, U.Member (State.S BS.ByteString) es ) =>
	Word8 -> Word8 -> Word8 -> Int -> RL.Length ->
	Eff.E es BS.ByteString RL.R ()
putLenDist b b1 b2 i0 l0 = State.gets (Triple.distance i0) >>= \d0 -> do
	State.modify (`Triple.update` b)
	let	c1 = (l0 + 1, [RL.LenDist (l0 + 3) d0])
	(_mb1, _mb2, mb3) <- get3
	case mb3 of
		Nothing -> State.modify (`Triple.update` b1) >> proceed c1
		Just b3 -> State.get >>= \st ->
			Triple.indexLength st b1 b2 b3 getAhead >>= \case
				Nothing -> do
					State.modify (`Triple.update` b1)
					proceed c1
				Just (i, l) -> do
					d <- State.gets $ Triple.distance i
					State.modify (`Triple.update` b1)
					proceed if l0 >= l then c1 else
						(l + 2, [
							RL.Literal b,
							RL.LenDist (l + 3) d ])

proceed :: (
	Foldable t,
	U.Member Pipe.P es,
	U.Member (State.S Triple.T) es, U.Member (State.S AheadPos) es,
	U.Member (State.S BS.ByteString) es ) =>
	(Int, t o) -> Eff.E es BS.ByteString o ()
proceed (l', ys) = getBytes l' >>= \bs -> do
	State.modify \st -> foldl Triple.update st $ BS.unpack bs
	Pipe.yield `mapM_` ys

get3 :: (
	U.Member Pipe.P es,
	U.Member (State.S AheadPos) es, U.Member (State.S BS.ByteString) es ) =>
	Eff.E es BS.ByteString o (Maybe Word8, Maybe Word8, Maybe Word8)
get3 = (,,) <$> get <*> getAhead <*> getAhead

get :: (
	U.Member Pipe.P es, U.Member (State.S AheadPos) es,
	U.Member (State.S BS.ByteString) es ) =>
	Eff.E es BS.ByteString o (Maybe Word8)
get = State.gets BS.uncons >>= \case
	Nothing -> bool (pure Nothing) get =<< readMore
	Just (b, bs) -> Just b <$ (State.put (AheadPos 0) >> State.put bs)

getBytes :: (
	U.Member Pipe.P es, U.Member (State.S AheadPos) es,
	U.Member (State.S BS.ByteString) es ) =>
	Int -> Eff.E es BS.ByteString o BS.ByteString
getBytes n = State.get >>= \bs -> if BS.length bs >= n
	then BS.take n bs <$ State.put (BS.drop n bs)
	else readMore >> getBytes n

getAhead :: (
	U.Member Pipe.P es, U.Member (State.S AheadPos) es,
	U.Member (State.S BS.ByteString) es ) =>
	Eff.E es BS.ByteString o (Maybe Word8)
getAhead = State.get >>= \bs -> State.get >>= \(AheadPos i) ->
	case bs BS.!? i of
		Nothing -> bool (pure Nothing) getAhead =<< readMore
		Just b -> Just b <$ State.modify nextAheadPos

newtype AheadPos = AheadPos Int deriving Show

nextAheadPos :: AheadPos -> AheadPos
nextAheadPos (AheadPos p) = AheadPos $ p + 1

readMore ::
	(U.Member Pipe.P es, U.Member (State.S BS.ByteString) es) =>
	Eff.E es BS.ByteString o Bool
readMore = Pipe.isMore >>= bool (pure False)
	(True <$ (State.modify . flip BS.append =<< Pipe.await))
