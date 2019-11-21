module Lib where

import GHC.Stack (HasCallStack)
import System.IO.Unsafe

myHead :: HasCallStack => [a] -> a
myHead [] = error "myHead: bad"
myHead (x : _) = unsafePerformIO $ putStrLn "myHead called" >> pure x

timesDo :: Monad m => Int -> m () -> m ()
n `timesDo` _ | n < 1 = return ()
n `timesDo` act = act >> (n - 1) `timesDo` act

timesEvaluate :: Monad m => Int -> (a -> m ()) -> a -> m ()
timesEvaluate n _ _ | n < 1 = return ()
timesEvaluate n act x = act x >> timesEvaluate (n - 1) act x

evaluateOneTime :: IO ()
evaluateOneTime = 15 `timesDo` print (myHead "Hello")

evaluateManyTimes :: IO ()
evaluateManyTimes = timesEvaluate 15 (print . myHead) "Hello"
