{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Fail where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Control.Exception

run :: Monad m => Eff.E '[Fail.F, U.FromFirst m] i o a -> m (Either String a)
run = Eff.runM . Fail.run

runIO :: Eff.E '[Fail.F, Except.E ErrorCall, IO.I] i o a -> IO a
runIO = Eff.runM
	. Except.runIO . Fail.runExc ErrorCall (\(ErrorCall str) -> str)

sample0 :: MonadFail m => m ()
sample0 = do
	fail "foobar"

catch :: (U.Member Fail.F es, U.Base IO.I es) =>
	Eff.E es i o () -> Eff.E es i o ()
catch = (`Fail.catch` \msg -> IO.putStrLn $ "FAIL OCCUR: " ++ msg)
