{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments, ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Except where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Control.Exception

runIO :: Exception e => Eff.E '[Except.E e, IO.I] i o a -> IO a
runIO = Eff.runM . Except.runIO

runM :: Monad m => Eff.E '[Except.E e, U.FromFirst m] i o a -> m (Either e a)
runM = Eff.runM . Except.run

type ExceptErrorCallIO = ExceptIO ErrorCall
type ExceptIO e = '[Except.E e, IO.I]

sampleIo :: (
	U.Member (Except.E ErrorCall) es,
	U.Base IO.I es ) =>
	Eff.E es i o ()
sampleIo = do
	IO.putStrLn "Hello, world!"
	_ <- Except.throw $ ErrorCall "Hello"
	IO.putStrLn "Good-bye, world!"

sampleCatch :: (U.Member (Except.E ErrorCall) es, U.Base IO.I es) =>
	Eff.E es i o () -> Eff.E es i o ()
sampleCatch = (`Except.catch` \(e :: ErrorCall) -> IO.putStrLn $ "ERROR OCCUR: " ++ show e)

sampleFromIO :: (U.Member (Except.E SomeException) es, U.Base IO.I es) => Eff.E es i o String
sampleFromIO = Except.fromIO (type SomeException) $ readFile "foobar"
