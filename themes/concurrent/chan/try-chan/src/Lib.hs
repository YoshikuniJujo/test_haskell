module Lib where

import Control.Concurrent.Chan
import System.IO.Unsafe

import Control.Monad.STM
import Control.Concurrent.STM.TChan

sampleChan :: Chan Int
sampleChan = unsafePerformIO newChan

sampleTChan :: TChan Int
sampleTChan = unsafePerformIO newTChanIO
