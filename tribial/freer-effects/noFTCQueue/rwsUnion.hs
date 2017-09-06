{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Typeable
import Data.Monoid

import Freer
import UnionNoType

type Eff = Freer Union

data Reader e a where
	Reader :: Reader e e

ask :: Typeable e => Eff e
ask = Join (toUnion Reader) Pure

runReader0 :: forall e a . Typeable e => Eff a -> e -> a
runReader0 m e = case m of
	Pure x -> x
	Join u q -> case fromUnion u of
		Just (Reader :: Reader e b) -> runReader0 (q e) e
		Nothing -> error "bad"

runReader :: forall e a . Typeable e => Eff a -> e -> Eff a
runReader m e = case m of
	Pure x -> Pure x
	Join u q -> case fromUnion u of
		Just (Reader :: Reader e b) -> runReader (q e) e
		Nothing -> Join u $ (`runReader` e) . q

data Writer w a where
	Writer :: w -> Writer w ()

tell :: Typeable w => w -> Eff ()
tell w = Join (toUnion $ Writer w) Pure

runWriter0 :: forall w a . (Monoid w, Typeable w) => Eff a -> (a, w)
runWriter0 = \case
	Pure x -> (x, mempty)
	Join u q -> case fromUnion u of
		Just (Writer w :: Writer w b) -> second (w <>) . runWriter0 $ q ()
		Nothing -> error "bad"

runWriter :: forall w a . (Monoid w, Typeable w) => Eff a -> Eff (a, w)
runWriter = \case
	Pure x -> Pure (x, mempty)
	Join u q -> case fromUnion u of
		Just (Writer w :: Writer w b) -> second (w <>) <$> runWriter (q ())
		Nothing -> Join u $ runWriter . q

run :: Eff a -> a
run (Pure x) = x
