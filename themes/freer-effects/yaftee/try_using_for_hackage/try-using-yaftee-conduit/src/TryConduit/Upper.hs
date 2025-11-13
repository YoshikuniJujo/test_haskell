{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryConduit.Upper where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Char
import System.IO

run :: Eff.E '[Pipe.P] i o r -> Maybe r
run = Eff.run . Pipe.run

upper :: U.Member Pipe.P es => Eff.E es String String r
upper = PipeT.convert (toUpper <$>)

toUpperStrings :: [String] -> [String]
toUpperStrings ss = Eff.run . PipeL.to $ PipeL.from ss Pipe.=$= upper

toUpperFile :: FilePath -> IO [String]
toUpperFile fp = do
	h <- openFile fp ReadMode
	ss <- Eff.runM . PipeL.to $
		PipeIO.hGetLines h Pipe.=$= upper Pipe.=$= fix \go ->
			Pipe.awaitMaybe >>= \case
				Nothing -> pure ()
				Just s -> Pipe.yield s >> go
	hClose h
	pure ss

toUpperFile' :: FilePath -> IO ()
toUpperFile' fp = do
	h <- openFile fp ReadMode
	_ <- Eff.runM . Pipe.run $
		PipeIO.hGetLines h Pipe.=$= upper Pipe.=$= fix \go ->
			Pipe.awaitMaybe >>= \case
				Nothing -> pure ()
				Just s -> IO.putStr "> " >> IO.print s >> go
	hClose h

sample1 :: [String]
sample1 = toUpperStrings ["Hello", "World", "Foo", "Bar", "Baz"]

foo :: FilePath -> IO ()
foo fp = do
	h <- openFile fp ReadMode
	_ <- Eff.runM . Pipe.run $ PipeIO.hGetLines h Pipe.=$= PipeIO.print
	hClose h
