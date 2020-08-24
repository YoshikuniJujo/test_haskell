{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TrySharing where

import Control.Concurrent
import Control.Moffy
import Control.Moffy.NoThreadId
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

runSharingShowButton4 :: IO (Or (Or String String) (Or String String))
runSharingShowButton4 = runTagged do
	sb <- tag showButton
	sb' <- tag $ sb `first'` sb
	pure . runMouseEv $ sb' `first'` sb'

runSharingShowButton8 :: IO (Or (Or (Or String String) (Or String String)) (Or (Or String String) (Or String String)))
runSharingShowButton8 = runTagged do
	sb <- tag showButton
	sb' <- tag $ sb `first'` sb
	sb'' <- tag $ sb' `first'` sb'
	pure . runMouseEv $ sb'' `first'` sb''

runSharingShowButton2Button2 :: IO (Or (String, String) (String, String))
runSharingShowButton2Button2 = runTagged $ do
	sb <- tag showButton
	let	sb2 = (,) <$> sb <*> sb
	pure . runMouseEv $ sb2 `first` sb2

message :: String -> IO ()
message msg = putStrLn . ("\n" ++) . (msg ++) . (": " ++) . show =<< getCurrentTime

runMouseEv :: React s MouseEv r -> IO r
runMouseEv r = do
	f <- openField "RUN REACT" [exposureMask, buttonPressMask]
	interpretReact (retry $ handle Nothing f) r <* closeField f
