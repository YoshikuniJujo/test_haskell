{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Pipe.ByteString where

import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union
import Data.Bool
import Data.ByteString qualified as BS
import System.IO

hGet :: Union.Base IO effs =>
	Int -> Handle -> Eff.E (Pipe.P i BS.ByteString ': effs) ()
hGet bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>=
	bool (pure ()) (Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield >> go)

putStr :: Union.Base IO effs =>
	Eff.E (Pipe.P BS.ByteString o ': effs) ()
putStr = fix \go -> Pipe.await >>= \case
	Nothing -> pure ()
	Just bs -> Eff.effBase (BS.putStr bs) >> go
