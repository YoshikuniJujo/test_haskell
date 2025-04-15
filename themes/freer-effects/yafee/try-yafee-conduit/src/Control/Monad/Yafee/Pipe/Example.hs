{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Pipe.Example where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Pipe.Tools qualified as Pipe
import Control.OpenUnion qualified as Union
import Data.Bool
import Data.Char
import System.IO

-- * EXAMPLES

hRead :: Union.Member IO effs => Handle -> Eff.E (Pipe.P () String ': effs) ()
hRead h = fix \go -> Eff.eff (not <$> hIsEOF h) >>=
	bool (pure ()) ((Pipe.yield =<< Eff.eff (hGetLine h)) >> go)

writeString :: Union.Member IO effs => Eff.E (Pipe.P String () ': effs) ()
writeString = Pipe.await >>= \case
	Nothing -> pure (); Just s -> Eff.eff (putStrLn s) >> writeString

takeP :: Int -> Eff.E (Pipe.P a a ': effs) ()
takeP 0 = pure ()
takeP n = Pipe.await >>= \case
	Nothing -> pure (); Just x -> Pipe.yield x >> takeP (n - 1)

baz :: IO ()
baz = void . Eff.runM . Pipe.run $
	hRead stdin Pipe.=$=
	takeP 3 Pipe.=$=
	Pipe.convert (map toUpper) Pipe.=$=
	writeString
