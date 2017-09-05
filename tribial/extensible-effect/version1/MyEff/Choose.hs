{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Choose where

import Control.Monad (join)

import MyEff
import Free
import TypeLevel

data Choose v = forall a . Choose [a] (a -> v)

instance Functor Choose where
	fmap f (Choose lst k) = Choose lst (f . k)

choose :: Member Choose r => [a] -> Eff r a
choose lst = Free . toUnion $ Choose lst Pure

mzero :: Member Choose r => Eff r a
mzero = choose []

mplus :: Member Choose r => Eff r a -> Eff r a -> Eff r a
mplus m1 m2 = join $ choose [m1, m2]

runChoice :: forall a r . Eff (Choose :> r) a -> Eff r [a]
runChoice = \case
	Pure x -> Pure [x]
	Free u -> case fromUnion u of
		Just (Choose lst f') -> concat <$> mapM (runChoice  . f') lst
		Nothing -> Free . fmap runChoice $ castUnion u
