{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckSharing where

import Control.Concurrent
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Type.Set
import Data.Or
import System.IO.Unsafe

import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Delete
import Control.Moffy.Handle.XField
import Control.Monad.Freer.Par
import Field

type MouseEv' = DeleteEvent :- MouseEv

heavy :: () -> Int
heavy _ = unsafePerformIO $ 123 <$ (putStrLn "START" >> threadDelay 2000000 >> putStrLn "END")

heavyReact :: () -> React s MouseEv' Int
heavyReact u = pure $ heavy u

heavyReact' :: () -> React s MouseEv' Int
heavyReact' u = do
	adjust leftClick
	pure $ heavy u

runMouseReact :: React s (DeleteEvent :- MouseEv) r -> IO r
runMouseReact r = do
	f <- openField "RUN REACT" [exposureMask]
	interpretReact (retry $ handle Nothing f) r
		<* closeField f

runSharingHeavy :: IO (Or Int Int)
runSharingHeavy = do
	f <- openField "RUN SHARING HEAVY" [exposureMask]
	r <- runTagged $ do
		hr <- tag $ heavyReact ()
		pure $ interpretReact (retry $ handle Nothing f) $ hr `first` hr
	r <$ closeField f

runNoSharingHeavy :: IO (Or Int Int)
runNoSharingHeavy = runMouseReact $ heavyReact () `first` heavyReact ()

runSharingHeavyHeavy :: IO (Or (Int, Int) (Int, Int))
runSharingHeavyHeavy = do
	f <- openField "RUN SHARING HEAVY HEAVY" [buttonPressMask, exposureMask]
	r <- runTagged $ do
		hr <- tag $ (,) <$> heavyReact () <*> heavyReact ()
		pure $ interpretReact (retry $ handle Nothing f) $ hr `first` hr
	r <$ closeField f

runSharingHeavy' :: IO (Or Int Int)
runSharingHeavy' = do
	f <- openField "RUN SHARING HEAVY" [buttonPressMask, exposureMask]
	r <- runTagged $ do
		hr <- tag $ heavyReact' ()
		pure $ interpretReact (retry $ handle Nothing f) $ hr `first` hr
	r <$ closeField f

runSharingHeavyHeavy' :: IO (Or (Int, Int) (Int, Int))
runSharingHeavyHeavy' = do
	f <- openField "RUN SHARING HEAVY HEAVY" [buttonPressMask, exposureMask]
	r <- runTagged $ do
		hr <- tag $ (,) <$> heavyReact' () <*> heavyReact' ()
		pure $ interpretReact (retry $ handle Nothing f) $ hr `first` hr
	r <$ closeField f
