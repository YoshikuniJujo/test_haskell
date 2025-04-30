{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.UseFTCQ.WriterO where

import Control.Arrow
import Control.Monad.Fix
import Data.Kind
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

data W (f :: Type -> Type -> Type -> Type) i o a where
	Tell :: o -> a -> W f i o a

instance HFunctor.HFunctor W where
	hmap _ g (Tell o x) = Tell o $ g x

instance HFunctor.HFunctorI W where
	hmapI _ g (Tell o x) = Tell o $ g x

tell :: Union.Member W effs => o -> Eff.E effs i o ()
tell = Eff.effh . (`Tell` ())

run :: (
	HFunctor.HFunctorO (Union.U effs),
	Monoid w
	) =>
	Eff.E (W ': effs) i w a -> Eff.E effs i w (w, a)
run = fix \go -> \case
	HFreer.Pure x -> HFreer.Pure (mempty, x)
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.hmapO run (mempty ,) u' HFreer.:>>=
			Q.singleton \(w, x) -> ((w <>) `first`) <$> go (q `HFreer.app` x)
		Right (Tell o x) -> ((o <>) `first`) <$> (go `HFreer.comp` q) x

sample :: (
	Union.Member W effs,
	Union.Base (Union.FromFirst IO) effs
	) =>
	Eff.E effs i String ()
sample = Eff.effBase getLine >>= tell
