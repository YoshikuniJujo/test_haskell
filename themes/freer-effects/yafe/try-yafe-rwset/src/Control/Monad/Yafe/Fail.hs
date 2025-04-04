{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Yafe.Fail where

import Control.Monad.Yafe.Eff qualified as Eff
import Control.OpenUnion qualified as Union

runFail :: Eff.E (Union.Fail ': effs) a -> Eff.E effs (Either String a)
runFail =
	Eff.handleRelay (pure . Right) \(Union.Fail msg) -> const . pure $ Left msg

runFailIO :: Union.Member IO effs => Eff.E (Union.Fail ': effs) a -> Eff.E effs a
runFailIO =
	Eff.handleRelay pure \(Union.Fail msg) -> const . Eff.eff $ fail @IO msg

tryFail :: (
	Union.Member Union.Fail effs,
	Union.Member IO effs ) => Eff.E effs ()
tryFail = do
	c : cs <- Eff.eff getLine
	Eff.eff $ print c
