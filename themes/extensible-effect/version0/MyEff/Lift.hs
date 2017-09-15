{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Lift (Lift, runLift, lift) where

import Control.Monad.Cont hiding (lift)
import Data.Typeable

import MyEff

data Lift m v = forall a . Lift (m a) (a -> v)
	deriving Typeable

instance Functor (Lift m) where
	fmap f (Lift m k) = Lift m (f . k)

lift :: (MemberU2 Lift (Lift m) r, Typeable m) =>
	m b -> Cont (VE r a) b
lift m = cont $ toEffect . Lift m

runLift :: (Monad m, Typeable m) => Cont (VE (Lift m :> ()) a) a -> m a
runLift m = lloop (runCont m V)

lloop :: (Monad m, Typeable m) => VE (Lift m :> ()) a -> m a
lloop m = case m of
	V x -> return x
	E _ -> case fromEffect m of
		Just (Lift m' k) -> m' >>= lloop . k
		Nothing -> error "cannot occur"
