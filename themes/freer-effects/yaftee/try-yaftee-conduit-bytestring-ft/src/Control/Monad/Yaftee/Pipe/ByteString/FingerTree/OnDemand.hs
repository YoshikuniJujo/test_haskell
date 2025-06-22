{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bool
import Data.ByteString.FingerTree qualified as BSF

data Request
	= RequestBytes Int
	| RequestBuffer Int
	| RequestString
	deriving Show

newtype ByteString = ByteString { unByteString :: BSF.ByteString } deriving Show

takeBytes :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Int -> Eff.E es BSF.ByteString o (Maybe BSF.ByteString)
takeBytes nm ln = State.getsN nm unByteString >>= \bs ->
	case BSF.splitAt' ln bs of
		Nothing -> readMore nm >>= bool (pure Nothing) (takeBytes nm ln)
		Just (t, d) -> Just t <$ State.putN nm (ByteString d)

readMore :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Eff.E es BSF.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm
		(ByteString . (`BSF.append` bs) . unByteString)
