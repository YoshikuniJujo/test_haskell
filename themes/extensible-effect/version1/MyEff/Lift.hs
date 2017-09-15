{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Lift where

import Data.Typeable

import MyEff
import Free
import TypeLevel

lift :: (Typeable m, BaseLift (Lift m) es) => m a -> Eff es a
lift m = Free . toUnion $ Lift m Pure

runLift :: (Monad m, Typeable m) => Eff (Lift m :> Base) a -> m a
runLift f = case f of
	Pure x -> return x
	Free u -> case fromUnion u of
		Just (Lift m' k) -> m' >>= runLift . k
		Nothing -> error "cannot occur"
