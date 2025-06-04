{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bool
import Data.Int
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.Png
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. (flip (State.runN @"foobar") $ ByteString LBS.empty)
		. (flip (State.runN @"foobar") $ Crc32 Crc.initialCrc32)
		. Except.run @String . Pipe.run . (`Except.catch` IO.putStrLn) . void $
		PipeLBS.hGet 64 h Pipe.=$= do
			readFileHeader "foobar"
			Pipe.yield =<< readRest "foobar"
			forever $ Pipe.yield =<< Pipe.await
		Pipe.=$= PipeIO.print

readFileHeader :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es LBS.ByteString o ()
readFileHeader nm = do
	ph <- readBytes nm 8
	when (ph /= fileHeader) $ Except.throw @String "PNG File header error"

readBytes :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (Except.E String) es ) =>
	Int64 -> Eff.E es LBS.ByteString o LBS.ByteString
readBytes nm n = State.getsN nm (LBS.splitAt' n . unByteString) >>= \case
	Nothing -> readMore nm
		>>= bool (Except.throw @String "no more ByteString") (readBytes nm n)
	Just (t, d) -> t <$ do
		State.modifyN nm $ Crc32 . (`Crc.crc32StepBS'` t) . unCrc32
		State.putN nm (ByteString d)

readRest :: forall (nm :: Symbol) ->
	U.Member (State.Named nm ByteString) es => Eff.E es i o LBS.ByteString
readRest nm = State.getsN nm unByteString

newtype ByteString = ByteString { unByteString :: LBS.ByteString } deriving Show
newtype Crc32 = Crc32 { unCrc32 :: Crc.Crc32 } deriving Show

readMore :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Eff.E es LBS.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm (`appendByteString` bs)

appendByteString :: ByteString -> LBS.ByteString -> ByteString
appendByteString (ByteString bs1) bs2 = ByteString $ bs1 `LBS.append` bs2
