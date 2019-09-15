{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.StateRW (Reader, Writer, ask, tell, modify, runStateR) where

import MyEff.Internal (Eff, Member, Freer(..), tsingleton, qComp, decomp)
import MyEff.Reader.Internal (Reader(..), ask)
import MyEff.Writer.Internal (Writer(..), tell)

modify :: (Member (Reader s) effs, Member (Writer s) effs) =>
	(s -> s) -> Eff effs ()
modify = (=<< ask) . (tell .)

runStateR :: Eff (Writer s ': Reader s ': effs) a -> s -> Eff effs (a, s)
runStateR = flip loop
	where
	loop :: s -> Eff (Writer s ': Reader s ': effs) a -> Eff effs (a, s)
	loop s = \case
		Pure x -> return (x, s)
		Join u q -> case decomp u of
			Right (Writer o) -> f o ()
			Left u' -> case decomp u' of
				Right Reader -> f s s
				Left u'' -> Join u'' . tsingleton $ f s
			where f = (q `qComp`) . loop
