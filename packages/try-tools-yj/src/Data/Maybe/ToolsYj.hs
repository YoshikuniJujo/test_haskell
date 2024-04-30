{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Maybe.ToolsYj (orErrorIO, findMaybe, findMaybeM) where

orErrorIO :: String -> Maybe a -> IO a
orErrorIO msg = maybe (error msg) pure

findMaybe :: (a -> Maybe b) -> [a] -> Maybe (a, b)
findMaybe prd = \case
	[] -> Nothing
	p : ps -> ($ prd p) \case
		Nothing -> findMaybe prd ps; Just x -> Just (p, x)

findMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe (a, b))
findMaybeM prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findMaybeM prd ps; Just x -> pure $ Just (p, x)
