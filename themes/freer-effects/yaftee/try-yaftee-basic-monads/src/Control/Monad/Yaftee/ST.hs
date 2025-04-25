{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.ST (

	S, new, read, write, modify, modify', run

	) where

import Prelude hiding (read)
import Control.Monad.ST qualified as ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.STRef qualified as ST

type (S s) = Union.FromFirst (ST.ST s)

new :: Union.Base (S s) effs => a -> Eff.E effs (ST.STRef s a)
new = Eff.effBase . ST.newSTRef

read :: Union.Base (S s) effs => ST.STRef s a -> Eff.E effs a
read = Eff.effBase . ST.readSTRef

write :: Union.Base (S s) effs => ST.STRef s a -> a -> Eff.E effs ()
write = (Eff.effBase .) . ST.writeSTRef

modify, modify' ::
	Union.Base (S s) effs => ST.STRef s a -> (a -> a) -> Eff.E effs ()
modify = (Eff.effBase .) . ST.modifySTRef
modify' = (Eff.effBase .) . ST.modifySTRef'

run :: (forall s . Eff.E '[S s] a) -> a
run m = ST.runST $ Eff.runM m
