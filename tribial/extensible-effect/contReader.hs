{-# LANGUAGE GADTs, DeriveDataTypeable, DeriveFunctor #-}
{-# LANGUAGE TypeOperators, KindSignatures #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont
import Data.Typeable

newtype Id x = Id x

data Union r v where
	Union :: (Functor t, Typeable t) => Id (t v) -> Union r v

data VE w r = Val w | E (Union r (VE w r))

newtype Reader e v = Reader (e -> v) deriving (Typeable, Functor)

ask :: Typeable e => Cont (VE w (Reader e :> r)) e
ask = cont $ E . Union . Id . Reader

runReader :: Typeable e =>
	Cont (VE w (Reader e :> r)) w -> e -> Cont (VE w r) w
runReader m e = case admin m of
	Val x -> return x
	E u -> case decomp u of
		Right (Reader x) -> case x e of
			Val x' -> return x'
			E u' -> undefined
		Left u -> undefined

data State s w = State (s -> s) (s -> w) deriving (Typeable, Functor)

modify :: Typeable s => (s -> s) -> Cont (VE r w) s
modify f = cont $ E . Union . Id . State f

decomp :: Typeable t => Union (t :> r) v -> Either (Union r v) (t v)
decomp (Union v) | Just (Id x) <- gcast1 v = Right x
decomp (Union v) = Left (Union v)

infixr 1 :>
data ((a :: * -> *) :> b)

admin :: Cont (VE w r) w -> VE w r
admin = ($ Val) . runCont

run :: Cont (VE w r) w -> w
run m = case admin m of Val x -> x
