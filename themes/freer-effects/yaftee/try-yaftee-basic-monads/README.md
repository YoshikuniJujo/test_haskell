# try-yaftee-basic-monads

## Reader, Writer and State

### Reader

```Haskell:TryYaftee/Reader.hs
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Reader where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Reader qualified as Reader
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

run :: Monad m => Eff.E '[Reader.R Int, U.FromFirst m] i o a -> m a
run = Eff.runM . (`Reader.run` 123)

sample :: (U.Member (Reader.R Int) es, U.Base IO.I es) => Eff.E es i o ()
sample = do
	e <- Reader.ask @Int
	IO.print e
	Reader.local @Int (* 2) do
		e' <- Reader.ask @Int
		IO.print e'
```

### Writer

```Haskell:TryYaftee/Writer.hs
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Writer where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Writer qualified as Writer
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

action :: IO ((), [String])
action = run @[String] getLines

run :: Monoid w => Eff.E '[Writer.W w, IO.I] i o r -> IO (r, w)
run = Eff.runM . Writer.run

getLines :: (U.Member (Writer.W [String]) es, U.Base IO.I es) => Eff.E es i o ()
getLines = IO.getLine >>= \ln ->
	when (not $ null ln) (Writer.tell [ln] >> getLines)
```

### State

```Haskell:TryYaftee/State.hs
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.State where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Reader qualified as Reader
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U

sample :: ((), Int)
sample = run @Int 3 5 $ increaseNTimes 7

run :: d -> a -> Eff.E '[Reader.R d, State.S a] i o r -> (r, a)
run d x0 = Eff.run . (`State.run` x0) . (`Reader.run` d)

increase ::
	(U.Member (Reader.R Int) es, U.Member (State.S Int) es) =>
	Eff.E es i o ()
increase = do
	d <- Reader.ask @Int
	State.modify (+ d)

increaseNTimes :: (U.Member (Reader.R Int) es, U.Member (State.S Int) es) =>
	Int -> Eff.E es i o ()
increaseNTimes n = replicateM_ n increase
```

## Except and Fail

### Except

```Haskell:TryYaftee/Except.hs
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.State where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Reader qualified as Reader
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U

sample :: ((), Int)
sample = run @Int 3 5 $ increaseNTimes 7

run :: d -> a -> Eff.E '[Reader.R d, State.S a] i o r -> (r, a)
run d x0 = Eff.run . (`State.run` x0) . (`Reader.run` d)

increase ::
	(U.Member (Reader.R Int) es, U.Member (State.S Int) es) =>
	Eff.E es i o ()
increase = do
	d <- Reader.ask @Int
	State.modify (+ d)

increaseNTimes :: (U.Member (Reader.R Int) es, U.Member (State.S Int) es) =>
	Int -> Eff.E es i o ()
increaseNTimes n = replicateM_ n increase
```

### Fail

```Haskell:TryYaftee/Fail.hs
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Fail where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Control.Exception

run :: Monad m => Eff.E '[Fail.F, U.FromFirst m] i o a -> m (Either String a)
run = Eff.runM . Fail.run

runIO :: Eff.E '[Fail.F, Except.E ErrorCall, IO.I] i o a -> IO a
runIO = Eff.runM
	. Except.runIO . Fail.runExc ErrorCall (\(ErrorCall str) -> str)

sample0 :: MonadFail m => m ()
sample0 = do
	fail "foobar"

catch :: (U.Member Fail.F es, U.Base IO.I es) =>
	Eff.E es i o () -> Eff.E es i o ()
catch = (`Fail.catch` \msg -> IO.putStrLn $ "FAIL OCCUR: " ++ msg)
```

## NonDet

## Base Monad

### IO

### ST

## Trace
