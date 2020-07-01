{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckSharing where

import Control.Concurrent
import Data.Or
import System.IO.Unsafe

import Moffy.React
import Moffy.React.Common
import Moffy.Handle
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse
import Freer
import Field

heavy :: () -> Int
heavy _ = unsafePerformIO $ 123 <$ (putStrLn "START" >> threadDelay 2000000 >> putStrLn "END")

heavyReact :: () -> React s MouseEv Int
heavyReact u = pure $ heavy u

heavyReact' :: () -> React s MouseEv Int
heavyReact' u = do
	adjust leftClick
	pure $ heavy u

runMouseReact :: React s MouseEv r -> IO r
runMouseReact r = do
	f <- openField "RUN REACT" [exposureMask]
	interpretReact (retry $ handleMouse Nothing f) r
		<* closeField f

runSharingHeavy :: IO (Or Int Int)
runSharingHeavy = do
	f <- openField "RUN SHARING HEAVY" [exposureMask]
	r <- runCount $ do
		hr <- addTag $ heavyReact ()
		pure $ interpretReact (retry $ handleMouse Nothing f) $ hr `first` hr
	r <$ closeField f

runNoSharingHeavy :: IO (Or Int Int)
runNoSharingHeavy = runMouseReact $ heavyReact () `first` heavyReact ()

runSharingHeavyHeavy :: IO (Or (Int, Int) (Int, Int))
runSharingHeavyHeavy = do
	f <- openField "RUN SHARING HEAVY HEAVY" [buttonPressMask, exposureMask]
	r <- runCount $ do
		hr <- addTag $ (,) <$> heavyReact () <*> heavyReact ()
		pure $ interpretReact (retry $ handleMouse Nothing f) $ hr `first` hr
	r <$ closeField f

runSharingHeavy' :: IO (Or Int Int)
runSharingHeavy' = do
	f <- openField "RUN SHARING HEAVY" [buttonPressMask, exposureMask]
	r <- runCount $ do
		hr <- addTag $ heavyReact' ()
		pure $ interpretReact (retry $ handleMouse Nothing f) $ hr `first` hr
	r <$ closeField f

runSharingHeavyHeavy' :: IO (Or (Int, Int) (Int, Int))
runSharingHeavyHeavy' = do
	f <- openField "RUN SHARING HEAVY HEAVY" [buttonPressMask, exposureMask]
	r <- runCount $ do
		hr <- addTag $ (,) <$> heavyReact' () <*> heavyReact' ()
		pure $ interpretReact (retry $ handleMouse Nothing f) $ hr `first` hr
	r <$ closeField f
