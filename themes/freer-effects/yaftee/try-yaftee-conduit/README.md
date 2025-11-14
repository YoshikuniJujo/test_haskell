# try-yaftee-conduit

```Haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryConduit.FromOld where

import Prelude hiding (take, putStrLn)

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Char
import System.IO (openFile, IOMode(..), hClose)

action :: FilePath -> IO ()
action fp = do
	h <- openFile fp ReadMode
	_ <- Eff.runM . Pipe.run $
		PipeIO.hGetLines h Pipe.=$=
		take 5 Pipe.=$=
		PipeT.convert (toUpper <$>) Pipe.=$=
		putStrLn
	hClose h

take :: U.Member Pipe.P es => Int -> Eff.E es a a ()
take = \case
	0 -> pure ()
	n -> Pipe.await >>= \x -> Pipe.yield x >> take (n - 1)

putStrLn :: (U.Member Pipe.P es, U.Base IO.I es) => Eff.E es String o r
putStrLn = forever $ IO.putStrLn =<< Pipe.await
```
