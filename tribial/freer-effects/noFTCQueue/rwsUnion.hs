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

run :: Eff a -> a
run (Pure x) = x
run _ = error "bad"

runM :: (Typeable m, Monad m) => Eff a -> m a
runM (Pure x) = return x
runM (Join u q) = case fromUnion u of
	Just m -> m >>= runM . q
	Nothing -> error "bad"

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

data State s a where
	Get :: State s s
	Put :: s -> State s ()

get :: Typeable s => Eff s
get = Join (toUnion Get) Pure

put :: Typeable s => s -> Eff ()
put s = Join (toUnion $ Put s) Pure

modify :: Typeable s => (s -> s) -> Eff ()
modify f = fmap f get >>= put

runState0 :: forall s a . Typeable s => Eff a -> s -> (a, s)
runState0 m s = case m of
	Pure x -> (x, s)
	Join u q -> case fromUnion u of
		Just (Get :: State s b) -> runState0 (q s) s
		Just (Put s' :: State s b) -> runState0 (q ()) s'
		Nothing -> error "bad"

runState :: forall s a . Typeable s => Eff a -> s -> Eff (a, s)
runState m s = case m of
	Pure x -> Pure (x, s)
	Join u q -> case fromUnion u of
		Just (Get :: State s b) -> runState (q s) s
		Just (Put s' :: State s b) -> runState (q ()) s'
		Nothing -> Join u $ (`runState` s) . q
