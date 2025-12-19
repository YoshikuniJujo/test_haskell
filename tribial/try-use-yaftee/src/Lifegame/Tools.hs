{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Tools where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U

div' :: Integral n => n -> n -> n
a `div'` b = (a - 1) `div` b + 1

pop :: forall nm -> (U.Member (State.Named nm [a]) es) => Eff.E es i o (Maybe a)
pop nm = State.getsModifyN nm \case [] -> Nothing; x : xs -> Just (x, xs)
