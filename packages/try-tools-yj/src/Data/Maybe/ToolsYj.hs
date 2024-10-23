{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Maybe.ToolsYj (
	forceJust, forceJust', orErrorIO, findMaybe, findMaybeM ) where

import Control.Exception

forceJust :: Exception e => e -> Maybe a -> a
forceJust e = maybe (throw e) id

forceJust' :: String -> Maybe a -> a
forceJust' = forceJust . ErrorCall

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
