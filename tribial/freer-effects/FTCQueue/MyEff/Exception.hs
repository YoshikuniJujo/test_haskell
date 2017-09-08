{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Exception (Exc, runError, throwError, catchError) where

import MyEff (Eff, Member, send, handleRelay, interpose)

newtype Exc e a = Exc e

throwError :: Member (Exc e) effs => e -> Eff effs a
throwError = send . Exc

runError :: Eff (Exc e ': effs) a -> Eff effs (Either e a)
runError = handleRelay (pure . Right) $ \(Exc e) _ -> pure $ Left e

catchError :: Member (Exc e) effs =>
	Eff effs a -> (e -> Eff effs a) -> Eff effs a
m `catchError` handle = interpose pure (\(Exc e) _ -> handle e) m
