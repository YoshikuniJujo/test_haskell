{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
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
import Control.Moffy (React, adjust)
import Control.Moffy.NoThreadId qualified as NT (first)
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow (MouseEv, mouseDown)
import Control.Moffy.Handle (retry, liftHandle', mergeSt, retrySt)
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretReact, interpretReactSt)
import Control.Concurrent (threadDelay)
import Data.Type.Set
import Data.Or (Or)
import Data.Time (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

import Field (openField, closeField, exposureMask, buttonPressMask)

import Control.Moffy.Event.Window

---------------------------------------------------------------------------

-- * NO SHARING
-- * SHARING
--	+ SIMPLE
--	+ NEST FIRST'
--	+ TWO TIME CLICK
-- * TOOLS
--	+ TYPE OR'
--	+ RUN MOUSE EV
--	+ SHOW BUTTON

---------------------------------------------------------------------------
-- NO SHARING
---------------------------------------------------------------------------

runShowButton2 :: IO (Or' String, Maybe WindowId)
runShowButton2 = runMouseEv $ showButton `NT.first` showButton

---------------------------------------------------------------------------
-- SHARING
---------------------------------------------------------------------------

-- SIMPLE

runSharingShowButton2 :: IO ((Or' String), Maybe WindowId)
runSharingShowButton2 =
	runTagged $ tag showButton >>= \sb -> pure . runMouseEv $ sb `NT.first` sb

-- NEST FIRST'

runSharingShowButton4 :: IO (Or' (Or' String), Maybe WindowId)
runSharingShowButton4 = runTagged do
	sb <- tag showButton
	sb' <- tag $ sb `NT.first` sb
	pure . runMouseEv $ sb' `NT.first` sb'

runSharingShowButton8 :: IO (Or' (Or' (Or' String)), Maybe WindowId)
runSharingShowButton8 = runTagged do
	sb <- tag showButton
	sb' <- tag $ sb `NT.first` sb
	sb'' <- tag $ sb' `NT.first` sb'
	pure . runMouseEv $ sb'' `NT.first` sb''

-- TWO TIME CLICK

runSharingShowButton2Button2 :: IO (Or' (String, String), Maybe WindowId)
runSharingShowButton2Button2 = runTagged $ do
	sb <- tag showButton
	let	sb2 = (,) <$> sb <*> sb
	pure . runMouseEv $ sb2 `NT.first` sb2

---------------------------------------------------------------------------
-- TOOLS
---------------------------------------------------------------------------

-- TYPE OR'

type Or' a = Or a a

-- RUN MOUSE EV

runMouseEv :: React s (LoadDefaultWindow :- MouseEv) r -> IO (r, Maybe WindowId)
runMouseEv r = do
	f <- openField "TRY SHARING" [exposureMask, buttonPressMask]
	interpretReactSt (retrySt $ handleDefaultWindow `mergeSt` liftHandle' (handle Nothing f)) r Nothing <* closeField f

-- SHOW BUTTON

showButton :: React s (LoadDefaultWindow :- MouseEv) String
showButton = adjust $ show' <$> mouseDown

show' :: Show a => a -> String
show' x = unsafePerformIO
	$ show x <$ (message "Start" >> threadDelay 2000000 >> message "END")

message :: String -> IO ()
message msg =
	putStrLn . ("\n" ++) . (msg ++) . (": " ++) . show =<< getCurrentTime
