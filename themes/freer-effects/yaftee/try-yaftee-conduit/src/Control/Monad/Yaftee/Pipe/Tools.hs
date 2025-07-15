{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Tools (

	-- * CONVERT

	convert, convert', convert'',

	-- * EITHER

	checkRight, skipLeft1,

	-- * LENGTH

	lengthRun, length, Length,

	-- * SCAN

	scanl

	) where

import Prelude hiding (length, scanl)
import Prelude qualified as P
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
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

lengthRun :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm Length ': es) i o a -> Eff.E es i o (a, Length)
lengthRun = (`State.runN` (0 :: Length))

length :: forall nm -> (
	Foldable t,
	U.Member Pipe.P es, U.Member (State.Named nm Length) es ) =>
	Eff.E es (t a) (t a) r
length nm = forever $ Pipe.await >>= \s ->
	State.modifyN nm (+ Length (P.length s)) >> Pipe.yield s

newtype Length = Length { unLength :: Int }
	deriving (Show, Eq, Bits, FiniteBits, Ord, Enum, Num, Real, Integral)

scanl :: U.Member Pipe.P es => (b -> a -> b) -> b -> Eff.E es a b r
scanl op =
	fix \go v -> (v `op`) <$>  Pipe.await >>= \v' -> Pipe.yield v' >> go v'
