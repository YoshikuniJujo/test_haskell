{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.ST (

	-- * TYPE

	S,

	-- * REF

	new, read, write, modify, modify',

	-- * RUN
	
	run

	) where

import Prelude hiding (read)
import Control.Monad.ST qualified as ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.STRef qualified as ST

type (S s) = Union.FromFirst (ST.ST s)

new :: forall s effs i o a .
	Union.Base (S s) effs => a -> Eff.E effs i o (ST.STRef s a)
new = Eff.effBase . ST.newSTRef

read :: forall s effs i o a .
	Union.Base (S s) effs => ST.STRef s a -> Eff.E effs i o a
read = Eff.effBase . ST.readSTRef

write :: forall s effs i o a .
	Union.Base (S s) effs => ST.STRef s a -> a -> Eff.E effs i o ()
write = (Eff.effBase .) . ST.writeSTRef

modify, modify' :: forall s effs i o a .
	Union.Base (S s) effs => ST.STRef s a -> (a -> a) -> Eff.E effs i o ()
modify = (Eff.effBase .) . ST.modifySTRef
modify' = (Eff.effBase .) . ST.modifySTRef'

run :: (forall s . Eff.E '[S s] i o a) -> a
run m = ST.runST $ Eff.runM m
