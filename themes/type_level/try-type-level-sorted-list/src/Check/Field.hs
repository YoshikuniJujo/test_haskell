{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.Field where

import Control.Monad
import Data.Time

import Field

checkTimeout :: DiffTime -> IO ()
checkTimeout t = do
	f <- openField "Check Timeout" [exposureMask]
	doWhile_ $ withNextEventTimeout' f (round $ t * 1000000) \case
		Just ExposeEvent {} -> pure True
		Just ButtonEvent {} -> pure False
		Just DestroyWindowEvent {} -> pure False
		Just ev	| isDeleteEvent f ev -> True <$ destroyField f
			| otherwise -> print ev >> pure True
		Nothing -> putStrLn "<TIMEOUT>" >> pure True
	closeField f

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	b <- act
	when b $ doWhile_ act
