{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Arrow
import Data.List.NonEmpty qualified as NE

spanR :: (a -> Bool) -> [a] -> Either [a] (NE.NonEmpty a, [a])
spanR p = \case
	[] -> Left []
	x : xs -> case (p x, spanR p xs) of
		(_, Right td) -> Right $ (x NE.<|) `first` td
		(False, Left d) -> Right (x NE.:| [], d)
		(True, Left d) -> Left $ x : d

takeR :: Int -> [a] -> [a]
takeR n xs = take (length xs - n) xs
