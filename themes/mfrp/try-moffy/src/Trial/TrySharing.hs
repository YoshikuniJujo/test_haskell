{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TrySharing where

import Control.Concurrent
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Or
import Data.Time
import System.IO.Unsafe

import Control.Moffy.Event.Mouse
import Control.Moffy.Handle.XField
import Control.Monad.Freer.Par
import Field

show' :: Show a => a -> String
show' x = unsafePerformIO
	$ show x <$ (message "Start" >> threadDelay 2000000 >> message "END")

showButton :: React s MouseEv String
showButton = adjust $ show' <$> mouseDown

runShowButton2 :: IO (Or String String)
runShowButton2 = runMouseEv $ showButton `first` showButton

runSharingShowButton2 :: IO (Or String String)
runSharingShowButton2 =
	runTagged $ tag showButton >>= \sb -> pure . runMouseEv $ sb `first` sb

calc :: () -> Int
calc _ = unsafePerformIO
	$ 123 <$ (message "START" >> threadDelay 2000000 >> message "END")

message :: String -> IO ()
message msg = putStrLn . ("\n" ++) . (msg ++) . (": " ++) . show =<< getCurrentTime

runMouseEv :: React s MouseEv r -> IO r
runMouseEv r = do
	f <- openField "RUN REACT" [exposureMask, buttonPressMask]
	interpretReact (retry $ handle Nothing f) r <* closeField f

calcReact :: () -> React s MouseEv Int
calcReact u = pure $ calc u

calcReact' :: () -> React s MouseEv Int
calcReact' u = adjust leftClick >> pure (calc u)

runCalc2 :: IO (Or Int Int)
-- runCalc2 = run $ calcReact () `first` calcReact ()
runCalc2 = runMouseEv $ hr `first` hr
	where hr = calcReact ()

runSharingCalc2 :: IO (Or Int Int)
runSharingCalc2 =
	runTagged $ tag (calcReact ()) >>= \hr -> pure . runMouseEv $ hr `first` hr

runSharingCalcCalc :: IO (Or (Int, Int) (Int, Int))
runSharingCalcCalc = do
	f <- openField "RUN SHARING HEAVY HEAVY" [buttonPressMask, exposureMask]
	r <- runTagged $ do
		hr <- tag $ (,) <$> calcReact () <*> calcReact ()
		pure $ interpretReact (retry $ handle Nothing f) $ hr `first` hr
	r <$ closeField f

runSharingCalc' :: IO (Or Int Int)
runSharingCalc' = do
	f <- openField "RUN SHARING HEAVY" [buttonPressMask, exposureMask]
	r <- runTagged $ do
		hr <- tag $ calcReact' ()
		pure $ interpretReact (retry $ handle Nothing f) $ hr `first` hr
	r <$ closeField f

runSharingCalcCalc' :: IO (Or (Int, Int) (Int, Int))
runSharingCalcCalc' = do
	f <- openField "RUN SHARING HEAVY HEAVY" [buttonPressMask, exposureMask]
	r <- runTagged $ do
		hr <- tag $ (,) <$> calcReact' () <*> calcReact' ()
		pure $ interpretReact (retry $ handle Nothing f) $ hr `first` hr
	r <$ closeField f
