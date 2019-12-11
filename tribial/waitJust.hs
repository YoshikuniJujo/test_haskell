{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Concurrent.STM

waitJust :: TVar (Maybe a) -> STM a
waitJust v = readTVar v >>= \case
	Nothing -> retry
	Just x -> pure x
