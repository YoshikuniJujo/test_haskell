{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bool
import Data.Int
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.ByteString.Lazy.BitArray qualified as BitArray

data Request
	= RequestBits Int
	| RequestBytes Int
	| RequestBuffer Int
	| RequestString
	| RequestPushBack BitArray.B
	deriving Show

newtype BitArray = BitArray { unBitArray :: BitArray.B } deriving Show

takeBits :: forall es o . forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Int64 -> Eff.E es LBS.ByteString o
		(Maybe (Either BitArray.B LBS.ByteString))
takeBits nm ln = State.getsN nm unBitArray >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore nm >>= bool (pure Nothing) (takeBits nm ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.putN nm (BitArray d)

splitString :: LBS.ByteString -> Maybe (LBS.ByteString, LBS.ByteString)
splitString bs = case LBS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, LBS.uncons -> Just (z, d)) -> Just (LBS.snoc t z, d)
	_ -> error "Never occur"

readMore :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Eff.E es LBS.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm
		(BitArray . (`BitArray.appendByteString` bs) . unBitArray)
