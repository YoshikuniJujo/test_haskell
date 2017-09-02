{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont
import Data.Typeable

data VE a
	= Val a
	| forall t . (Typeable t, Typeable a) => E (t (VE a))
	deriving Typeable

class Typeable e => Effect e where
	toEffect :: Typeable a => e (VE a) -> VE a
	fromEffect :: VE a -> Maybe (e (VE a))

	toEffect = E
	fromEffect = \case
		E e -> cast e
		Val _ -> Nothing

newtype Reader e v = Reader (e -> v)
	deriving Typeable

instance Typeable e => Effect (Reader e) where

ask :: (Typeable e, Typeable a) => Cont (VE a) e
ask = cont $ toEffect . Reader

runReader :: Typeable e => Cont (VE a) a -> e -> VE a
runReader m e = rloop (runCont m Val) e

rloop :: Typeable e => VE a -> e -> VE a
rloop m e = case m of
	Val x -> Val x
	E _u -> case fromEffect m of
		Just (Reader r) -> rloop (r e) e
		Nothing -> undefined

run :: VE a -> a
run (Val x) = x
run _ = undefined
