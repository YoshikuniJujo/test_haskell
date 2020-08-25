{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TrySharing (
	-- * No Sharing
	runShowButton2,
	-- * Sharing
	-- ** Simple
	runSharingShowButton2,
	-- ** Nest first'
	runSharingShowButton4, runSharingShowButton8,
	-- ** Two Time Click
	runSharingShowButton2Button2
	) where

import Control.Monad.Freer.Par (runTagged, tag)
import Control.Moffy (React, adjust, first)
import Control.Moffy.NoThreadId (first')
import Control.Moffy.Event.Mouse (MouseEv, mouseDown)
import Control.Moffy.Handle (retry)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretReact)
import Control.Concurrent (threadDelay)
import Data.Or (Or)
import Data.Time (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

import Field (openField, closeField, exposureMask, buttonPressMask)

---------------------------------------------------------------------------

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
