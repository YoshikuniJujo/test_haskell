module Main where

import Control.Monad
import Control.Moffy
import Trial.TryConfigureEvent
import Trial.Draw.Viewable

main :: IO ()
main = void $ runTryConfigure (\_ _ -> mapM_ putMessage) (prepare $ adjustSig tryConfigure)
