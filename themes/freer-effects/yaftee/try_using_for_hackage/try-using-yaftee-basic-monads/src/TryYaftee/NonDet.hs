{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.NonDet where

import Control.Applicative
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.NonDet qualified as NonDet

run :: (Traversable f, MonadPlus f) => Eff.E '[NonDet.N] i o r -> f r
run = Eff.run . NonDet.run

foo :: Alternative f => f Int
foo = pure 123

bar :: Alternative f => f Int
bar = empty

foobar :: Alternative f => f Int
foobar = foo <|> bar
