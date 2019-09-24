{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Exception (Exc, runError, throwError, catchError) where

import Eff (Eff, Member, send, Freer(..), decomp, prj)

newtype Exc e a = Exc e

throwError :: Member (Exc e) effs => e -> Eff effs a
throwError = send . Exc

runError :: Eff (Exc e ': effs) a -> Eff effs (Either e a)
runError = \case
	Pure x -> return $ Right x
	u `Bind` k -> case decomp u of
		Right (Exc e) -> return $ Left e
		Left u' -> u' `Bind` (runError . k)

catchError :: Member (Exc e) effs =>
	Eff effs a -> (e -> Eff effs a) -> Eff effs a
m `catchError` h = case m of
	Pure x -> Pure x
	u `Bind` k -> case prj u of
		Just (Exc e) -> h e
		Nothing -> u `Bind` ((`catchError` h) . k)
