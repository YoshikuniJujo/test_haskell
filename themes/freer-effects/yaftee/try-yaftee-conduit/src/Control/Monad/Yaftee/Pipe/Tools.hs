{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Tools (

	convert, convert', convert'', checkRight,

	skipLeft1

	) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Bool

convert :: U.Member Pipe.P effs => (a -> b) -> Eff.E effs a b r
convert f = fix \go -> Pipe.await >>= ((>> go) . Pipe.yield . f)

convert' :: U.Member Pipe.P effs => (a -> b) -> Eff.E effs a b ()
convert' f = fix \go -> Pipe.isMore
	>>= bool (pure ()) (Pipe.await >>= ((>> go) . Pipe.yield . f))

convert'' :: U.Member Pipe.P es => (Bool -> a -> b) -> a -> Eff.E es a b ()
convert'' f = fix \go p -> Pipe.isMore >>= bool
	(Pipe.yield (f True p))
	(Pipe.await >>= \x -> ((>> go x) . Pipe.yield $ f False p))

checkRight :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Eff.E es (Either a b) b r
checkRight = fix \go -> Pipe.await >>= (>> go)
	. either (const $ Except.throw "(Left _) exist") (Pipe.yield)

skipLeft1 :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Eff.E es (Either a b) o b
skipLeft1 = Pipe.await >>= \case
	Left _ -> Pipe.await >>= \case
		Left _ -> Except.throw @String "Not Right"
		Right x -> pure x
	Right x -> pure x
