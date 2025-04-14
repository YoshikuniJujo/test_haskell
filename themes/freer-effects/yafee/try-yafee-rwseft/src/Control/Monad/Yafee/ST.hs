{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.ST where

import Control.Monad.ST
import Control.Monad.Yafee.Eff qualified as Eff
import Control.OpenUnion qualified as Union
import Data.STRef (STRef)
import Data.STRef qualified as ST

newSTRef :: Union.Base (ST s) effs => a -> Eff.E effs (STRef s a)
newSTRef = Eff.effBase . ST.newSTRef

readSTRef :: Union.Base (ST s) effs => STRef s a -> Eff.E effs a
readSTRef = Eff.effBase . ST.readSTRef

writeSTRef :: Union.Base (ST s) effs => STRef s a -> a -> Eff.E effs ()
writeSTRef r = Eff.effBase . ST.writeSTRef r

modifySTRef :: Union.Base (ST s) effs => STRef s a -> (a -> a) -> Eff.E effs ()
modifySTRef r = Eff.effBase . ST.modifySTRef r

modifySTRef' :: Union.Base (ST s) effs => STRef s a -> (a -> a) -> Eff.E effs ()
modifySTRef' r = Eff.effBase . ST.modifySTRef' r
