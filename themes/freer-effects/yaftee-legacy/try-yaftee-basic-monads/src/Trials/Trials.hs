{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Trials where

import Control.Applicative
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.NonDet qualified as NonDet
import Control.HigherOpenUnion qualified as Union

moveKnight :: (
	Union.Member NonDet.N effs
	) =>
	(Int, Int) -> Eff.E effs (Int, Int)
moveKnight (c, r) = do
	(c', r') <- asum $ pure <$> [
		(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1),
		(c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2) ]
	guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
	pure (c', r')

in3 :: Union.Member NonDet.N effs =>
	(Int, Int) -> Eff.E effs (Int, Int)
in3 = moveKnight <=< moveKnight <=< moveKnight
