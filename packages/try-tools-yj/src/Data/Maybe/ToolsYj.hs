{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Maybe.ToolsYj (findMaybeM) where

findMaybeM :: Monad  m => (a -> m (Maybe b)) -> [a] -> m (Maybe (a, b))
findMaybeM prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findMaybeM prd ps; Just x -> pure $ Just (p, x)
