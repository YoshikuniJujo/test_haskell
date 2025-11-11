{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.ST (

	-- * TYPE

	S,

	-- * REF

	newRef, readRef, writeRef, modifyRef, modifyRef',

	-- * RUN
	
	run

	) where

import Prelude hiding (read)
import Control.Monad.ST qualified as ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.STRef qualified as ST

type (S s) = Union.FromFirst (ST.ST s)

newRef :: forall s effs i o a .
	Union.Base (S s) effs => a -> Eff.E effs i o (ST.STRef s a)
newRef = Eff.effBase . ST.newSTRef

readRef :: forall s effs i o a .
	Union.Base (S s) effs => ST.STRef s a -> Eff.E effs i o a
readRef = Eff.effBase . ST.readSTRef

writeRef :: forall s effs i o a .
	Union.Base (S s) effs => ST.STRef s a -> a -> Eff.E effs i o ()
writeRef = (Eff.effBase .) . ST.writeSTRef

modifyRef, modifyRef' :: forall s effs i o a .
	Union.Base (S s) effs => ST.STRef s a -> (a -> a) -> Eff.E effs i o ()
modifyRef = (Eff.effBase .) . ST.modifySTRef
modifyRef' = (Eff.effBase .) . ST.modifySTRef'

run :: (forall s . Eff.E '[S s] i o a) -> a
run m = ST.runST $ Eff.runM m
