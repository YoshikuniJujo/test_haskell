{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall
	-fno-warn-tabs
	-fno-warn-simplifiable-class-constraints #-}

import Control.Monad.Cont hiding (lift)

import EffLift
import EffList

data VE (r :: Effs) a = VE a deriving Show

lift :: OnlyLift (Lift m) r => m b -> Cont (VE r a) b
lift = undefined

runLift :: Monad m => Cont (VE (Lift m :> Emp) a) a -> b
runLift = undefined
