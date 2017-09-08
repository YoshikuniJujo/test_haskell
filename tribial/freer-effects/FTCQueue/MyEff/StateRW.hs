{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.StateRW where

import MyEff.Internal
import MyEff.Reader.Internal
import MyEff.Writer.Internal

modify :: (Member (Reader s) effs, Member (Writer s) effs) =>
	(s -> s) -> Eff effs ()
modify f = tell . f =<< ask

runStateR :: Eff (Writer s ': Reader s ': effs) a -> s -> Eff effs (a, s)
runStateR = flip loop
	where
	loop :: s -> Eff (Writer s ': Reader s ': effs) a -> Eff effs (a, s)
	loop s = \case
		Pure x -> return (x, s)
		Join u q -> case decomp u of
			Right (Writer o) -> k o ()
			Left u' -> case decomp u' of
				Right Reader -> k s s
				Left u'' -> Join u'' . tsingleton $ k s
			where k = (q `qComp`) . loop
