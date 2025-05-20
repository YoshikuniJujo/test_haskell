{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.UseArrowLike where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO

import Try.ArrowLike
import Try.ArrowLike.Choice

sample :: IO ()
sample = void . Eff.runM
		. (`State.run` (Nothing :: Maybe String))
		. (`State.run` (Nothing :: Maybe Int))
		. Except.run @String
		. Pipe.run
	$ PipeL.from [3 :: Int .. 15] Pipe.=$=
		(PipeT.convert show &&& PipeT.convert (`replicate` 'a')) Pipe.=$=
		PipeIO.print

sample' :: IO ()
sample' = void . Eff.runM
	. (`State.run` (rightEmpty :: Right String))
	. (`State.run` (leftEmpty :: Left (Maybe String)))
	. Except.run @String
	. Pipe.run
	$ void (PipeL.from [Left (3 :: Int), Right "hello", Right "world", Left 2, Left 8, Right "foo"] Pipe.=$=
--	$ void (PipeL.from [Left 1, Left 2, Left 3, Right "hello", Left 2, Left 111] Pipe.=$=
		(PipeT.convert (Just . (`replicate` 'a')) ||| PipeT.convert (Just . concat . replicate 3)) Pipe.=$=
		PipeIO.print) `Except.catch` IO.putStrLn
