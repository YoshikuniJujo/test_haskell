{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.ByteString.IO where

import Data.ByteString qualified as BS
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union
import System.IO

bufferSize :: Int
bufferSize = 100

fromHandle ::
	forall i ->
	(Union.Member (Pipe.P i BS.ByteString) effs, Union.Member IO effs) =>
	Handle -> Eff.E effs ()
fromHandle i h = do
	eof <- Eff.eff $ hIsEOF h
	if eof then pure () else do
		bs <- Eff.eff $ BS.hGetSome h bufferSize
		Pipe.yield @i bs
		fromHandle i h
