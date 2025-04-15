{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Yafee.Pipe.Tools where

import Prelude hiding (take)
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe

-- * TOOLS

convert :: (a -> b) -> Eff.E (Pipe.P a b ': effs) ()
convert f = fix \go -> Pipe.await >>= maybe (pure ()) ((>> go) . Pipe.yield . f)

take :: Int -> Eff.E (Pipe.P a a ': effs) ()
take = \case
	0 -> pure ()
	n -> Pipe.await >>= maybe (pure ()) ((>> take (n - 1)) . Pipe.yield)
