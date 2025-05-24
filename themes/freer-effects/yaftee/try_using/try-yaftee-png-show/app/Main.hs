{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Png.Decode
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.TypeLevel.List
import Data.ByteString qualified as BS
import Data.CairoImage.Internal
import System.IO
import System.Environment
import Graphics.Pipe.Write
import Graphics.Pipe.Draw

main :: IO ()
main = do
	fpi : fpo : _ <- getArgs
	h <- openFile fpi ReadMode
	img <- newImageMut @Argb32Mut 16 16
	writeDrawPipe Effs fpo img (Except.run . Fail.runExc id . pngRun) $
		PipeBS.hGet 64 h Pipe.=$=
		(void (png "chunk" "deflate" IO.print) `Except.catch` IO.print @String) Pipe.=$=
		PipeT.convert BS.tail Pipe.=$=
		drawCairoImageRgba32 IO img 16 16

type Effs = PngStates "chunk" "deflate" `Append` '[Fail.F, (Except.E String)]
